import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.chess._
import com.chess.parse._
import scala.util.parsing.combinator._

@RunWith(classOf[JUnitRunner])
class ParsingTest extends Spec with ShouldMatchers {
  describe("Piece placement") {
    it("should be properly parsed to instanciate a board") {
       	val fenIn = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R w Q a1 0 4"
       	val b = FenConvertor.parse(fenIn)
       	val fenOut = FenConvertor.format(b)
    	(fenIn) should be (fenOut) 
    }
  }
  


}
