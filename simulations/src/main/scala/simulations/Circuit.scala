package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }
  
  def directlink(input: Wire, output: Wire){
	  input addAction {
	    () => output.setSignal(input.getSignal)
	  }
  }
  
  
  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }
  
  
  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1sig = a1.getSignal
      val a2sig = a2.getSignal
      afterDelay(OrGateDelay) {
        output.setSignal(a1sig | a2sig)
      }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1r, a2r,outputr= new Wire
    inverter(a1, a1r)
    inverter(a2, a2r)
    andGate(a1r, a2r, outputr)
    inverter(outputr, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]){
	  val n = out.length
  
	  if(n==1) {
	    directlink(in, out.head)
	  }else{
		val left = out take n/2
		val right = out takeRight n/2	
		
		val inleft, inright = new Wire
		
		val ch = c.head
		val chr = new Wire
		
		inverter(ch, chr)
		andGate(in,chr,inleft)
		andGate(in, ch, inright)
		
		demux(inleft,c.tail,left)
		demux(inright,c.tail,right)
	  }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }
  
  
  def demuxExample {
    
    val in=new Wire
    val c1,c0 = new Wire
    val o3,o2,o1,o0 = new Wire
    val clist = List(c1,c0)
    val olist = List(o3,o2,o1,o0)
    
   
    in.setSignal(true)
    c1.setSignal(true)
    c0.setSignal(false)
    demux(in,clist,olist);
    run 
    
    for(x<-olist) println(x.getSignal)
    
    
  }
  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  //Circuit.andGateExample
  Circuit.demuxExample
}
