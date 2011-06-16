import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.chess._

@RunWith(classOf[JUnitRunner])
class BoardTest extends Spec with ShouldMatchers {
  describe("Board") {
    it("should be instanted properly") {
    	val l = Board()
    	l.piece(0,0).get should be (R('w'))
    	l.piece(1,3).get should be (O)    	
    	l.piece(6,7).get should be (N('b'))
    	l.piece(7,7).get should be (R('b'))
    	l.where(K('w')).get should be (4,0)

    }
  }
  
  describe("Dummy game") {
    it("should be working") {
      val f = Board().<->((4,1),(4,3))
      f.piece(4,1).get should be (O)
      f.piece(4,3).get should be (P('w'))
      val g= f.<->((4,6),(4,4))
      g.piece(4,6).get should be (O)
      g.piece(4,4).get should be (P('b'))
      val h= g.<->((6,0),(5,2))
      h.piece(6,0).get should be (O)
      h.piece(5,2).get should be (N('w'))
      val i= h.<->((6,7),(5,5))
      i.piece(6,7).get should be (O)
      i.piece(5,5).get should be (N('b'))
      val j= i.<->((5,0),(3,2))
      j.piece(5,0).get should be (O)
      j.piece(3,2).get should be (B('w'))
      val k= j.<->((5,5),(4,3))
      k.piece(5,5).get should be (O)
      k.piece(4,3).get should be (N('b'))
      val l= k.<->((3,0),(4,1))
      l.piece(3,0).get should be (O)
      l.piece(4,1).get should be (Q('w'))
    }
  }

  describe("Board") {
    it("is legal move") {
      val l = Board()
      l.legalMoves((0,0)).length should be (0)
      val h = l.<->((0,1),(0,3))
      val y = h.legalMoves(0,0)
      h.piece(0,0).get should be (R('w'))
      y should be (List((0,1),(0,2)))
      h.legal((0,0),(0,1)) should be (true)
      h.legal((0,0),(1,0)) should be (false)
      h.legalMoves(1,0) should be (List((2,2),(0,2)))
      val m = h.<->((1,0),(2,2))
      val n = m.<->((2,2),(3,4))
      n.legalMoves(3,4) should be (List((4,6),(2,6),(2,2),(4,2),(1,5),(1,3),(5,5),(5,3)))      
    }
  }

  describe("Board") {
    it("is check (checkmate)") {
      val l = Board()
      val m = l.<->((6,1),(6,3))
      val n = m.<->((4,6),(4,4))
      val o = n.<->((5,1),(5,2))
      val p = o.<->((3,6),(3,4))
      val q = p.<->((1,0),(2,2))
      q.check('w') should be (false)
      q.checkmate('w') should be (false)
      val r = q.<->((3,7),(7,3))
      r.printit
      r.check('w') should be (true)
      r.checkmate('w') should be (true)
      r.check('b') should be (false)
    }
  }


}
