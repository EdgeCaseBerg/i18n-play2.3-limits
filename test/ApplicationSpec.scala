package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._

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
  }
}