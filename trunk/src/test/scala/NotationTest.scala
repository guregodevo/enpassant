import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.chess._
import com.chess.pgn._
import scala.util.parsing.combinator._

@RunWith(classOf[JUnitRunner])
class ParsingTest extends Spec with ShouldMatchers {
  describe("Piece placement") {
    it("should be properly parsed to instanciate a board") {
       	val in = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R"
       	val res = PieceParser.run(in)
       	val l = res.get.asInstanceOf[List[List[Any]]]
       	def flatten(s:List[Any]):List[Piece] = {
       		s match {
       			case Nil => Nil
       			case s:List[List[Piece]] => s.flatten       			
       		}
       	}
       	val fres = l.map(x => flatten(x))

	    def order(cases:List[List[Piece]]):List[List[Piece]] = { 	
		  for { i <- List.range(0,8) } 
		   	yield List.range(0,8).reverse.map( j => cases(j)(i))		  
       	}
       	val b = Board(order(fres), 'w', false, true, false, false ,Some((0,0)),0,4)

       	b.printit
    	print(b.toFEN()) 
    }
  }
  


}
