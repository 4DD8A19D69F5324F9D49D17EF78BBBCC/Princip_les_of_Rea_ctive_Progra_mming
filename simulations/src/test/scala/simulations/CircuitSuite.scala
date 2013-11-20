package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate test") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")

    in1.setSignal(false)
    run
    assert(out.getSignal === false, "and 4")
  }

  test("orgate test") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")
    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")

    in1.setSignal(true)
    assert(out.getSignal === true, "or 4")
  }

  test("orgate2 test") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")
    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")

    in1.setSignal(true)
    assert(out.getSignal === true, "or 4")
  }
  
  
  test("demux test") {
    
    val in=new Wire
    val c1,c0 = new Wire
    val o3,o2,o1,o0 = new Wire
    val clist = List(c1,c0)
    val olist = List(o3,o2,o1,o0)
    
   
    in.setSignal(true)
    c1.setSignal(false)
    c0.setSignal(false)
    demux(in,clist,olist);
    run 
    
    def getresult = for(x<-olist) yield x.getSignal
    
    assert(getresult === List(false,false,false,true), "demux 1")
    c0.setSignal(true)
    run
    assert(getresult === List(false,false,true,false), "demux 2")
    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(getresult === List(false,true,false,false), "demux 3")
    c0.setSignal(true)
    run
    assert(getresult === List(true,false,false,false), "demux 4")
    
    
    in.setSignal(false)
    run
    assert(getresult === List(false,false,false,false), "demux false 1")
    c0.setSignal(false)
    run
    assert(getresult === List(false,false,false,false), "demux false 2")
    
    c1.setSignal(false)
    c0.setSignal(true)
    run
    assert(getresult === List(false,false,false,false), "demux false 3")
    c0.setSignal(false)
    run
    assert(getresult === List(false,false,false,false), "demux false 4")
  }
}
