package com.outr.jarheaven

import java.io.{BufferedReader, DataInputStream, File, InputStreamReader}
import java.net.URL
import java.util
import java.util.jar.JarFile

import coursier._
import coursier.util.{Gather, Task}

import scala.annotation.tailrec
import scala.collection.convert.Wrappers.IteratorWrapper
import scala.concurrent.ExecutionContext.Implicits.global

object Test {
  def main(args: Array[String]): Unit = {
    segmented()
  }

  def direct(): Unit = {
    val youiExample = Dependency(Module(org"io.youi", name"youi-example_2.12"), "0.9.8")
    val start = Resolution(
      Set(
        youiExample,
        Dependency(Module(org"org.jboss.xnio", name"xnio-nio"), "3.3.8.Final")
      )
    )
    val repositories = Seq(
      Cache.ivy2Local,
      MavenRepository("https://repo1.maven.org/maven2")
    )
    val fetch = Fetch.from(repositories, Cache.fetch[Task]())
    val resolution = start.process.run(fetch).unsafeRun()
    println(s"Dependencies: ${resolution.dependenciesOf(resolution.rootDependencies.head)}")

    val localArtifacts: Seq[Either[FileError, File]] =
      Gather[Task].gather(
        resolution.artifacts().map(Cache.file[Task](_).run)
      ).unsafeRun()

    val files = localArtifacts.toList.map {
      case Left(error) => throw new RuntimeException(error.message)
      case Right(file) => file
    }
    files.sortBy(_.getName).foreach(f => println(s"File: ${f.getName}"))

    val command = List("java", "-cp", files.map(_.getAbsolutePath).mkString(":"), "io.youi.example.ServerExampleApplication")
    val pb = new ProcessBuilder(command: _*)
    pb.inheritIO()
    val process = pb.start()
    val exitValue = process.waitFor()
    println(s"Exit value: $exitValue")
  }

  def segmented(): Unit = {
    val youiExample = Dependency(Module(org"io.youi", name"youi-example_2.12"), "0.9.8")
    val start = Resolution(
      Set(
        youiExample,
        Dependency(Module(org"org.jboss.xnio", name"xnio-nio"), "3.3.8.Final")
      )
    )
    val repositories = Seq(
      Cache.ivy2Local,
      MavenRepository("https://repo1.maven.org/maven2")
    )
    val fetch = Fetch.from(repositories, Cache.fetch[Task]())
    val resolution = start.process.run(fetch).unsafeRun()
    println(s"Dependencies: ${resolution.dependenciesOf(resolution.rootDependencies.head)}")
    val artifactMap = resolution.dependencyArtifacts().map {
      case (dependency, attributes, artifact) => artifact -> (dependency, attributes)
    }.toMap

    val artifacts = Gather[Task].gather(resolution.artifacts().map { artifact =>
      Cache.file[Task](artifact).run.map {
        case Left(error) => throw new RuntimeException(error.message)
        case Right(file) => {
          val (dependency, attributes) = artifactMap(artifact)
          ResolvedArtifact(artifact, dependency, attributes, file)
        }
      }
    }).unsafeRun().toList.sortBy(_.file.getName)
    artifacts.foreach { a =>
      val name = a.dependency.name
      val dependencies = resolution.dependenciesOf(a.dependency).map(_.name).toList
      val classLoader = new SmartClassLoader(name, a, dependencies)
      SmartClassLoader.register(classLoader)
    }

    val root = SmartClassLoader.byName(resolution.rootDependencies.head.name)
    Thread.currentThread().setContextClassLoader(root)
    println(s"Root: $root, Path: ${root.resolvedArtifact.file.getAbsolutePath}")

//    val resources = root.getResources("application.json").asScala
//    resources.foreach { url =>
//      println(s"URL: $url, Value: ${readURL(url)}")
//    }

    val mainClass = root.loadClass("io.youi.example.ServerExampleApplication")
    println(s"Main-Class: $mainClass")
    val mainMethod = mainClass.getMethod("main", classOf[Array[String]])
    println(s"Main-Method: $mainMethod")
    mainMethod.invoke(None.orNull, Array[String]())

//    val command = List("java", "-cp", files.map(_.getAbsolutePath).mkString(":"), "io.youi.example.ServerExampleApplication")
//    val pb = new ProcessBuilder(command: _*)
//    pb.inheritIO()
//    val process = pb.start()
//    val exitValue = process.waitFor()
//    println(s"Exit value: $exitValue")
  }

  implicit class DependencyExtras(dependency: Dependency) {
    lazy val name: String = s"${dependency.module.organization.value}:${dependency.module.name.value}:${dependency.version}"
  }

  private def readURL(url: URL): Unit = {
    val in = new BufferedReader(new InputStreamReader(url.openStream()))
    try {
      def recurse(): Unit = {
        Option(in.readLine()) match {
          case None => // Finished
          case Some(line) => {
            println(line)
            recurse()
          }
        }
      }

      recurse()
    } finally {
      in.close()
    }
  }
}

case class ResolvedArtifact(artifact: Artifact, dependency: Dependency, attributes: Attributes, file: File) {
  override def toString: String = s"File: ${file.getName}, Artifact: ${artifact.url}, Dependency: $dependency"
}

class SmartClassLoader(val name: String,
                       val resolvedArtifact: ResolvedArtifact,
                       val dependencies: List[String]) extends ClassLoader(null) {
  private lazy val jar = new JarFile(resolvedArtifact.file)
  private lazy val classLoaders = dependencies.map(SmartClassLoader.byName)
  private var classMap = Map.empty[String, Class[_]]

  override def findClass(name: String): Class[_] = classByName(name) match {
    case Some(c) => c
    case None => classLoaders.collectFirst {
      case cl if Option(cl.loadClass(name)).nonEmpty => cl.loadClass(name)
    }.getOrElse(SmartClassLoader.classByName(name).orNull)
  }

  def classByName(name: String): Option[Class[_]] = synchronized {
    classMap.get(name).orElse {
      Option(jar.getJarEntry(s"${name.replace('.', '/')}.class")).map { jarEntry =>
        val bytes = new Array[Byte](jarEntry.getSize.toInt)
        val input = new DataInputStream(jar.getInputStream(jarEntry))
        try {
          input.readFully(bytes)
          val c = defineClass(name, bytes, 0, bytes.length)
          classMap += name -> c
          c
        } finally {
          input.close()
        }
      }
    }
  }

  override def findResource(name: String): URL = {
    Option(super.findResource(name)).getOrElse {
      resource(name) match {
        case Some(u) => u
        case None => classLoaders.collectFirst {
          case cl if Option(cl.findResource(name)).nonEmpty => cl.findResource(name)
        }.getOrElse(SmartClassLoader.findResource(name))
      }
    }
  }

  override def findResources(name: String): util.Enumeration[URL] = {
    val resources = SmartClassLoader.findResources(name)
    IteratorWrapper(resources.toIterator)
  }

  def resource(name: String): Option[URL] = {
    Option(jar.getEntry(name)).map(_ => new URL(s"jar:${resolvedArtifact.file.toURI.toURL}!/$name"))
  }

  override def toString: String = name

  def dispose(): Unit = jar.close()
}

object SmartClassLoader {
  private var map = Map.empty[String, SmartClassLoader]

  def all: List[SmartClassLoader] = map.valuesIterator.toList

  def register(classLoader: SmartClassLoader): Unit = synchronized {
    map += classLoader.name -> classLoader
  }

  def byName(name: String): SmartClassLoader = map(name)

  def dispose(): Unit = map.values.foreach(_.dispose())

  @tailrec
  def classByName(className: String, classLoaders: List[SmartClassLoader] = all): Option[Class[_]] = {
    if (classLoaders.isEmpty) {
      None
    } else {
      val cl = classLoaders.head
      cl.classByName(className) match {
        case Some(c) => Some(c)
        case None => classByName(className, classLoaders.tail)
      }
    }
  }

  def findResource(name: String): URL = {
    all.collectFirst {
      case cl if Option(cl.findResource(name)).nonEmpty => cl.findResource(name)
    }.orNull
  }

  def findResources(name: String): List[URL] = all.flatMap { cl =>
    cl.resource(name)
  }.distinct
}