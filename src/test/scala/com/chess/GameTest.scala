import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec
import com.chess._

@RunWith(classOf[JUnitRunner])
class ChessBoardTest extends Spec with ShouldMatchers {
  describe("ChessBoard") {
    it("should be instanted properly") {
    	val l = ChessBoard()
    	l.piece(0,0).get should be (R('w'))
    	l.piece(1,3).get should be (O)    	
    	l.piece(6,7).get should be (N('b'))
    	l.piece(7,7).get should be (R('b'))
    	l.where(K('w')).get should be (4,0)
    }
  }
  
  describe("Dummy game") {
    it("should be working") {
      val f = ChessBoard().<->((4,1),(4,3))
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

  describe("ChessBoard") {
    it("is legal move") {
      val l = ChessBoard()
      l.legalMoves((0,0)).length should be (0)
      val h = l.<->((0,6),(0,5)).<->((0,1),(0,3))
      h.legalMoves(0,0) should be (List((0,1),(0,2)))
      
      h.legal((0,0),(0,1)) should be (true)
      h.legal((0,0),(1,0)) should be (false)
      h.legalMoves(1,0) should be (List((2,2),(0,2)))
      val m = h.<->((1,0),(2,2)).<->((2,2),(3,4))
      m.legalMoves(3,4) should be (List((4,6),(2,6),(2,2),(4,2),(1,5),(1,3),(5,5),(5,3)))      
    }
  }

  describe("ChessBoard") {
    it("is check or checkmate") {
      val l = ChessBoard()
      val m = l.<->((6,1),(6,3)).<->((4,6),(4,4)).<->((5,1),(5,2)).<->((3,6),(3,4)).<->((1,0),(2,2))
      m.check('w') should be (false)
      m.checkmate('w') should be (false)
      val n = m.<->((3,7),(7,3))
      n.check('w') should be (true)
      n.checkmate('w') should be (true)
      n.check('b') should be (false)
    }
  }

  describe("ChessBoard") {
    it("pawn move is legal or en passant") {
      val l = ChessBoard()
      val m = l.<->((6,1),(6,3)).<->((2,6),(2,4)).<->((6,3),(6,4)).<->((2,4),(2,3)).<->((0,1),(0,2)).<->((5,6),(5,4))
      m.legalMoves(6,4) should be (List((6,5), (5,5)))
      m.legalMoves(2,1) should be (List((2,2)))
      m.legalMoves(3,1) should be (List((3,2),(3,3)))
      m.legalMoves(0,6) should be (List((0,5),(0,4)))
      m.legalMoves(2,3) should be (List((2,2)))
      val n = m <-> ((2,3),(2,2))
      n.legalMoves(2,2) should be (List((3,1),(1,1)))
      n.legalMoves(1,1) should be (List((1,2),(1,3),(2,2)))
      val o = n.<-> ((4,1),(4,3))
      o.check('w') should be (false)
      val p = o.<->((2,2), (3,1))
      p.check('w') should be (true)
      p.legalMoves(4,0) should be (List((4,1),(3,1)))
      
    }
  }

  describe("ChessBoard") {
    it("stalemate") {
      val l = ChessBoard()
      val m = l.<->((6,1),(6,3)).<->((2,6),(2,4)).<->((6,3),(6,4)).<->((2,4),(2,3)).<->((5,6),(5,4)).<->((7,6),(7,5))
      m.legalMoves(6,4) should be (List((6,5), (7,5)))
      val n = m <-> ((2,3),(2,2))
      n.legalMoves(2,2) should be (List((3,1),(1,1)))
      n.legalMoves(1,1) should be (List((1,2),(1,3),(2,2)))
      val o = n.<-> ((4,1),(4,3))
      o.check('w') should be (false)
      val p = o.<->((2,2), (3,1))
      p.check('w') should be (true)
      p.legalMoves(4,0) should be (List((4,1),(3,1)))
      
    }
  }
  
  describe("ChessBoard") {
    it("Black and White kingside castling") {
      val l = ChessBoard()
      val m = l.<->((5,1),(5,3)).<->((6,6),(6,5)).<->((6,0),(5,2)).<->((0,6),(0,5)).<->((4,1),(4,2)).<->((5,7),(6,6))
      m.legalMoves(4,7) should be (List((5,7)))
      m.legalMoves(4,0) should be (List((4,1),(5,1)))
      val n = m.<->((5,0),(4,1)).<->((6,7),(5,5))
      
      n.legalMoves(4,0) should be (List((5,0),(5,1),(6,0)))
      
      val o = n.<->((4,0),(6,0))
      o.piece(5,0).get should be (R('w'))
      o.piece(6,0).get should be (K('w'))
      
      o.prettyPrint
      
      o.legalMoves(4,7) should be (List((5,7),(6,7)))
      
    }
  }
  
}
