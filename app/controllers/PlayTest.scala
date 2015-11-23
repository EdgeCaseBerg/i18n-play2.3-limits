package controllers

import play.api._
import play.api.mvc._
import play.api.i18n._
import play.api.http.HeaderNames
import scala.util.control.Exception.allCatch

class PlayTest extends Controller {
	
	def index = Action { implicit request =>
		val msg = Messages("index.msg")
    val langInHeader = request.headers.get(HeaderNames.ACCEPT_LANGUAGE).getOrElse("Not Set!")
		Ok(views.html.index(msg, langInHeader))
	}

  def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined

  def getStringQVal(s: String) =  s.replace("q=","")

  def supportAny = Action { request =>
    val langHeader = request.headers.get(HeaderNames.ACCEPT_LANGUAGE).getOrElse("en;q=1") 

    val lang = langHeader.split(",").map{ value => 
      Option(value.split(";").toList match {
        case tag :: Nil => (tag,1.0)
        case tag :: qVal :: Nil if isDoubleNumber(getStringQVal(qVal))  => (tag, getStringQVal(qVal).toDouble)
        case _ => null
      })
    }.filter(_.isDefined).map(_.get).sortWith(_._2 > _._2).head._1.toString

    implicit val language = Lang(lang)
    val msg = Messages("index.msg")
    Ok(views.html.index(msg, langHeader))
  }

}
