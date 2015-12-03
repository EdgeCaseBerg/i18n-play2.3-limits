package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.i18n.Lang

import controllers.AnyLangController

class ApplicationSpec extends Specification {

  
  "Application" should {
    "render the index page in en by default" in {
      running(FakeApplication()) {
        val home = route(FakeRequest(GET, "/")).get
        
        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/html")
        contentAsString(home) must contain ("<TD>Not Set!</TD>")
        contentAsString(home) must contain ("<TD>Hello!</TD>")
        contentAsString(home) must contain ("<TD>en</TD>")
      }
    }

    "render the index page in en-US" in {
    	running(FakeApplication()) {
    		val langHeader = "fr-ca,fr;q=0.8,en-us;q=1"
    		val home = route(FakeRequest(GET, "/").withHeaders( ("Accept-Language",langHeader))).get
        
        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/html")
        contentAsString(home) must contain (s"<TD>$langHeader</TD>")
        contentAsString(home) must contain ("<TD>What&#x27;s up!?</TD>")
        contentAsString(home) must contain ("<TD>en-US</TD>")
    	}
    }
    
    "render the index page in fr" in {
    	running(FakeApplication()) {
    		val langHeader = "fr-ca,fr;q=0.8,en-us;q=0.1"
    		val home = route(FakeRequest(GET, "/").withHeaders( ("Accept-Language",langHeader))).get
        
        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/html")
        contentAsString(home) must contain (s"<TD>$langHeader</TD>")
        contentAsString(home) must contain ("<TD>Bonjour</TD>")
        contentAsString(home) must contain ("<TD>fr</TD>")
    	}
    }

    "ignore an unsupported language in /" in {
    	running(FakeApplication()) {
    		val langHeader = "de-DE,de;q=0.8,en;q=0.1"
    		val home = route(FakeRequest(GET, "/").withHeaders( ("Accept-Language",langHeader))).get
        
        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/html")
        contentAsString(home) must contain (s"<TD>$langHeader</TD>")
        contentAsString(home) must contain ("<TD>Hello!</TD>")
        contentAsString(home) must contain ("<TD>en</TD>")
    	}
    }

    "attempt to support an unsupported language in /any but default to fallback from messages" in {
    	running(FakeApplication()) {
    		val langHeader = "de-DE,de;q=0.8,en;q=0.1"
    		val home = route(FakeRequest(GET, "/any").withHeaders( ("Accept-Language",langHeader))).get
        
        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/html")
        contentAsString(home) must contain (s"<TD>$langHeader</TD>")
        contentAsString(home) must contain ("<TD>Hello!</TD>") //won't show Wie Geht's despite messages.de-DE existing because it's not in the preferred languages conf
        contentAsString(home) must contain ("<TD>de-DE</TD>")
    	}
    }

    "render the index page in fr on /any" in {
    	running(FakeApplication()) {
    		val langHeader = "fr-ca,fr;q=0.8,en-us;q=0.1"
    		val home = route(FakeRequest(GET, "/any").withHeaders( ("Accept-Language",langHeader))).get
        
        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/html")
        contentAsString(home) must contain (s"<TD>$langHeader</TD>")
        contentAsString(home) must contain ("<TD>Bonjour</TD>")
        contentAsString(home) must contain ("<TD>fr-CA</TD>") //note we use first q=1 val of french and play doesn't mind and uses messages.fr
    	}
    }

    "render the index page and respect the q value of a language on /any" in {
    	running(FakeApplication()) {
    		val langHeader = "fr-ca;q=0.3,fr;q=0.2,en-us;q=0.5"
    		val home = route(FakeRequest(GET, "/any").withHeaders( ("Accept-Language",langHeader))).get
        
        status(home) must equalTo(OK)
        contentType(home) must beSome.which(_ == "text/html")
        contentAsString(home) must contain (s"<TD>$langHeader</TD>")
        contentAsString(home) must contain ("<TD>What&#x27;s up!?</TD>")
        contentAsString(home) must contain ("<TD>en-US</TD>") //note we use first q=1 val of french and play doesn't mind and uses messages.fr
    	}
    }

    "render the language it's passed to an AnyLangController" in {
        running(FakeApplication()) {
            val langHeader = "fr-ca;q=0.3,fr;q=0.2,en-us;q=0.5"
            val home = route(FakeRequest(GET, "/support").withHeaders( ("Accept-Language",langHeader))).get
        
            status(home) must equalTo(OK)
            contentType(home) must beSome.which(_ == "text/plain")
            contentAsString(home) must contain ("Lang(en,us)")
        }
    }

    "render the language it's passed even if not in application.conf" in {
        running(FakeApplication()) {
            val langHeader = "fr-ca;q=0.3,fr;q=0.2,en-us;q=0.5,de;"
            val home = route(FakeRequest(GET, "/support").withHeaders( ("Accept-Language",langHeader))).get
        
            status(home) must equalTo(OK)
            contentType(home) must beSome.which(_ == "text/plain")
            contentAsString(home) must contain ("Lang(de,)")
        }   
    }

    object FakeController extends AnyLangController

    "parse a RFC 2616 compliant header with one language" in {
        val expectedLang = Lang("da", "")
        val header = "da, en-gb;q=0.8, en;q=0.7"
        FakeController.rfc2616ParseHeader(header) must equalTo(expectedLang) 
    }

    "parse a RFC 2616 compliant header with multiple languages" in {
        val expectedLang = Lang("en", "gb")
        val header = "da;q=0.1, en-gb;q=0.8, en;q=0.7"
        FakeController.rfc2616ParseHeader(header) must equalTo(expectedLang) 
    }

    /** 
     * The BNF says:      Accept-Language = 1#( language-range [ weight ] ) 
     * Which means that we have a list of ranges of at least 1 and as many as 
     * we want. The full form of # lists are "<n>#<m>element" indicating at 
     * least <n> and at most <m> elements, each separated by one or more commas
     * (",") and OPTIONAL linear white space (LWS). Because the spec also says:
     * "A recipient MAY replace any linear white space with a single SP before 
     * interpreting the field value or forwarding the message downstream." We 
     * are at liberty to ignore runs of whitespace and treat them as one. 
     * tl;dr you can put as much whitespace as you want between language tags
     * @see http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html
     */
    "parse a RFC 2616 compliant header with multiple languages and whitespace" in {
        val expectedLang = Lang("en", "gb")
        val header = "da;q=0.1,    en-gb;q=0.8,     en;q=0.7"
        FakeController.rfc2616ParseHeader(header) must equalTo(expectedLang) 
    }

    "parse a RFC 2616 compliant header with no q values" in {
        val expectedLang = Lang("de", "")
        val header = "de,da, en-gb, en"
        FakeController.rfc2616ParseHeader(header) must equalTo(expectedLang) 
    }

    "parse a RFC 2616 compliant header with all q values specified" in {
        val expectedLang = Lang("fr", "ca")
        val header = "da;q=0.1, en-gb;q=0.3,fr-ca;q=0.9, en;q=0.5"
        FakeController.rfc2616ParseHeader(header) must equalTo(expectedLang) 
    }

    /**
     * If the header field is present in a request and none of the available 
     * representations for the response have a matching language tag, the origin 
     * server can either disregard the header field by treating the response as 
     * if it is not subject to content negotiation or honor the header field by
     * sending a 406 (Not Acceptable) response.  However, the latter is not
     * encouraged, as doing so can prevent users from accessing content that
     * they might be able to use (with translation software, for example).
     * @see http://tools.ietf.org/html/rfc7231#section-5.3.5
     */
    "return a default if an empty header is sent" in {
        val expectedLang = play.api.i18n.Lang.defaultLang
        val header = ""
        FakeController.rfc2616ParseHeader(header) must equalTo(expectedLang) 
    }
  }
}