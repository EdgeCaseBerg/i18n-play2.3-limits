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

  def rfc2616ParseHeader(headerString: String): Lang = {
    val parsedLanguagesAndQvalues = headerString.split(",").map { value =>
      Option(value.trim.split(";").toList match {
        case tag :: Nil =>
          /* Each language-range MAY be given an associated quality value which represents an estimate of the user's preference for the languages specified by that range. The quality value defaults to "q=1". */
          (tag.trim, 1.0)
        case tag :: qVal :: Nil if isDoubleNumber(getStringQVal(qVal)) => (tag.trim, getStringQVal(qVal).toDouble)
        case _ => null
      })
    }

    val languageCode = parsedLanguagesAndQvalues.filter(_.isDefined) //remove any unparseable values
      .map(_.get) //Option[Tuple2] -> Tuple2(Tag, Q-Val)
      .filterNot(_._1.isEmpty)
      .sortWith(_._2 > _._2) //Order by Q Value
      .headOption //Handle empty list just in case
      .getOrElse((play.api.i18n.Lang.defaultLang.code, 1.0))
      ._1.toString //return language code   
    Lang(languageCode)
  }

  def supportAny = Action { request =>
    val langHeader = request.headers.get(HeaderNames.ACCEPT_LANGUAGE).getOrElse("en;q=1") 
    implicit val language = rfc2616ParseHeader(langHeader)
    val msg = Messages("index.msg")
    Ok(views.html.index(msg, langHeader))
  }
}

class AnyLangSupportedController extends AnyLangController {

  def showLang(implicit lang: Lang) : String ={
    lang.toString
  }

  def ping = Action { implicit request =>
    Ok(showLang)
  }
}

trait AnyLangController extends Controller {

  def isDoubleNumber(s: String): Boolean = (allCatch opt s.toDouble).isDefined

  def getStringQVal(s: String) =  s.replace("q=","")

    def rfc2616ParseHeader(headerString: String): Lang = {
    val parsedLanguagesAndQvalues = headerString.split(",").map { value =>
      Option(value.trim.split(";").toList match {
        case tag :: Nil =>
          /* Each language-range MAY be given an associated quality value which represents an estimate of the user's preference for the languages specified by that range. The quality value defaults to "q=1". */
          (tag.trim, 1.0)
        case tag :: qVal :: Nil if isDoubleNumber(getStringQVal(qVal)) => (tag.trim, getStringQVal(qVal).toDouble)
        case _ => null
      })
    }

    val languageCode = parsedLanguagesAndQvalues.filter(_.isDefined) //remove any unparseable values
      .map(_.get) //Option[Tuple2] -> Tuple2(Tag, Q-Val)
      .filterNot(_._1.isEmpty)
      .sortWith(_._2 > _._2) //Order by Q Value
      .headOption //Handle empty list just in case
      .getOrElse((play.api.i18n.Lang.defaultLang.code, 1.0))
      ._1.toString //return language code   
    Lang(languageCode)
  }

  override implicit def request2lang(implicit request: RequestHeader) : Lang = {
    play.api.Play.maybeApplication.map { implicit app =>
      val langHeader = request.headers.get(HeaderNames.ACCEPT_LANGUAGE).getOrElse(play.api.i18n.Lang.defaultLang.toString)
      val maybeLangFromCookie = request.cookies.get(Play.langCookieName).flatMap(c => Lang.get(c.value))
      maybeLangFromCookie.getOrElse(rfc2616ParseHeader(langHeader))
    }.getOrElse(play.api.i18n.Lang.defaultLang)
  }
}
