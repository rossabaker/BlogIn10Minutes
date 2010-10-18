package com.meetup.scala

trait BlogDAO {
  def getBlog: Blog

  def postComment(id: Int, comment: Comment): Unit
}

object MemoryBlogDao extends BlogDAO {
  val comment1 = Comment("My comment", "jboyens")
  val comment2 = Comment("My anon comment")

  val entries = List(
    Entry(1, "This is my entry text", List(comment1, comment2)),
    Entry(2, "This is hacker blogging", List(comment1))
  )

  val blog = Blog(entries)

  def getBlog = blog

  def postComment(id: Int, comment: Comment) = {
    val entry = entries.find(_.id == id)

    if (entry.isDefined) {
      entry.get.comments = comment :: entry.get.comments
    }
  }
}

import org.scalaquery.ql.{Join, Query, Projection, ColumnBase, AbstractTable}
import org.scalaquery.ql.TypeMapper._
import org.scalaquery.ql.basic.BasicDriver
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.ql.basic.{BasicTable => Table}
import org.scalaquery.util.NamingContext
import org.scalaquery.session._

object ScalaQueryBlogDao extends BlogDAO {
  val db = Database.forURL("jdbc:postgresql:blog-in-10-minutes", driver = "org.postgresql.Driver")

  object Entries extends Table[(Int, String)]("entries") {
    def id = column[Int]("id")
    def content = column[String]("content")
    def * = id ~ content

    def findAll()(implicit session: Session): List[Entry] = {
      val query = for {
        e <- Entries
        c <- Comments if c.entryId is e.id
      } yield (e.id ~ e.content ~ c.text ~ c.username)

      // TODO this sucks.
      query.list
        .map {row => (Entry(row._1, row._2, Nil), Comment(row._3, row._4))}
        .foldLeft(List[Entry]()) { case(entries, result) =>
          def addComment(entries: List[Entry]): List[Entry] = entries match {
            case (x :: xs) if (x.id == result._1.id) =>
              Entry(result._1.id, result._1.content, result._2 :: x.comments) :: xs
            case (x :: xs) =>
              x :: addComment(xs)
            case Nil =>
              List(Entry(result._1.id, result._1.content, List(result._2)))
          }
          addComment(entries)
        }
    }
  }

  object Comments extends Table[(Int, Int, String, String)]("comments") {
    def id = column[Int]("id")
    def entryId = column[Int]("entry_id")
    def text = column[String]("content")
    def username = column[String]("username")
    def * = id ~ entryId ~ text ~ username
  }

  def getBlog() = db withSession { session => Blog(Entries.findAll()(session)) }

  def postComment(entryId: Int, comment: Comment) = error("not implemented yet")
}
