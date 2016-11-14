import pir.graph
import pir.graph._
import pir.graph.enums._
import pir.codegen._
import pir.plasticine.config._
import pir.Design
import pir.misc._
import pir.PIRApp

object LogRegDesign extends PIRApp {
  override val arch = SN_4x4
  def main(args: String*)(top:Top) = {
    val x4818_vector = Vector("x4818")
    val x4817_scalar = Scalar("x4817")
    val x4615_argin = ArgIn("x4615")
    val bus_731_scalar = Scalar("bus_731")
    val bus_758_scalar = Scalar("bus_758")
    val x4820_vector = Vector("x4820")
    val x4815_scalar = Scalar("x4815")
    val x4816_scalar = Scalar("x4816")
    val x4720_scalar = Scalar("x4720")
    val x4821_vector = Vector("x4821")
    val x4819_vector = Vector("x4819")
    val x4702_vector = Vector("x4702")
    val x4822_scalar = Scalar("x4822")
    val x4825_scalar = Scalar("x4825")
    val x4823_scalar = Scalar("x4823")
    val x4765_scalar = Scalar("x4765")
    val x4622_oc = OffChip("x4622")
    val x4621_oc = OffChip("x4621")
    val x4719_scalar = Scalar("x4719")
    val x4824_scalar = Scalar("x4824")
    val x4814_scalar = Scalar("x4814")
    val bus_973_vector = Vector("bus_973")
    val x4764_scalar = Scalar("x4764")
    val bus_733_scalar = Scalar("bus_733")
    val x4614_argin = ArgIn("x4614")
    val x4710_vector = Vector("x4710")
    val x4620_oc = OffChip("x4620")
    val x4786_mc = MemoryController(TileLoad, x4621_oc)
    val x4741_mc = MemoryController(TileLoad, x4620_oc)
    val x5078_mc = MemoryController(TileStore, x4622_oc)
    val x5082 = UnitPipeline(name = "x5082", parent=top, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val x5082_unitcc = CounterChain(name = "x5082_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      var stage: List[Stage] = Nil
    }
    val x5060 = Sequential(name = "x5060", parent=x5082, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val ctr1 = (Const("0i").out, CU.scalarIn(stage0, x4614_argin).out, Const("1i").out) // Counter
      val x4705 = CounterChain(name = "x4705", ctr1)
      var stage: List[Stage] = Nil
    }
    val x5058 = Sequential(name = "x5058", parent=x5060, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val ctr3 = (Const("0i").out, Const("1i").out, Const("1i").out) // Counter
      val x4707 = CounterChain(name = "x4707", ctr3)
      val ctr5 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4709 = CounterChain(name = "x4709", ctr5)
      var stage: List[Stage] = Nil
    }
    val x5038 = MetaPipeline(name = "x5038", parent=x5058, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val ctr6 = (Const("0i").out, CU.scalarIn(stage0, x4615_argin).out, Const("96i").out) // Counter
      val x4713 = CounterChain(name = "x4713", ctr6)
      var stage: List[Stage] = Nil
    }
    val x4762 = MetaPipeline(name = "x4762", parent=x5038, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val ctr8 = (Const("0i").out, Const("96i").out, Const("1i").out) // Counter
      val x4718 = CounterChain(name = "x4718", ctr8)
      var stage: List[Stage] = Nil
    }
    val x4737 = StreamController(name = "x4737", parent=x4762, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val x4713 = CounterChain.copy(x5038, "x4713")
      val x4737_unitcc = CounterChain(name = "x4737_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      val x4718 = CounterChain.copy(x4762, "x4718")
      var stage: List[Stage] = Nil
    }
    val x4737_0 = StreamPipeline(name = "x4737_0", parent=x4737, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr729 = CU.temp
      val x4713 = CounterChain.copy(x5038, "x4713")
      val x4737_unitcc = CounterChain.copy(x4737, "x4737_unitcc")
      val x4718 = CounterChain.copy(x4762, "x4718")
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(2)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4713(0)), CU.ctr(stage(0), x4718(0))), op=FixAdd, results=List(CU.temp(stage(1), tr729)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr729), Const("192i")), op=FixMul, results=List(CU.scalarOut(stage(2), bus_731_scalar)))
    }
    val x4737_1 = StreamPipeline(name = "x4737_1", parent=x4737, deps=List(x4737_0)) { implicit CU => 
      val stage0 = CU.emptyStage
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(1)
      Stage(stage(1), operands=List(CU.scalarIn(stage(0), bus_731_scalar), Const("96i")), op=FixMod, results=List(CU.scalarOut(stage(1), x4719_scalar), CU.scalarOut(stage(1), bus_733_scalar)))
    }
    val x4737_2 = StreamPipeline(name = "x4737_2", parent=x4737, deps=List(x4737_0, x4737_1)) { implicit CU => 
      val stage0 = CU.emptyStage
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(1)
      Stage(stage(1), operands=List(CU.scalarIn(stage(0), bus_731_scalar), CU.scalarIn(stage(0), bus_733_scalar)), op=FixSub, results=List(CU.scalarOut(stage(1), x4741_mc.ofs)))
    }
    val x4737_3 = StreamPipeline(name = "x4737_3", parent=x4737, deps=List(x4737_1)) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr743 = CU.temp
      val tr742 = CU.temp
      val tr740 = CU.temp
      val tr739 = CU.temp
      val tr737 = CU.temp
      val x4713 = CounterChain.copy(x5038, "x4713")
      val x4737_unitcc = CounterChain.copy(x4737, "x4737_unitcc")
      val x4718 = CounterChain.copy(x4762, "x4718")
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(6)
      Stage(stage(1), operands=List(CU.scalarIn(stage(0), bus_733_scalar), Const("192i")), op=FixAdd, results=List(CU.scalarOut(stage(1), x4720_scalar), CU.temp(stage(1), tr737)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr737), Const("96i")), op=FixMod, results=List(CU.temp(stage(2), tr739)))
      Stage(stage(3), operands=List(CU.temp(stage(2), tr737), CU.temp(stage(2), tr739)), op=FixSub, results=List(CU.temp(stage(3), tr740)))
      Stage(stage(4), operands=List(CU.temp(stage(3), tr739), Const("0i")), op=FixNeq, results=List(CU.temp(stage(4), tr742)))
      Stage(stage(5), operands=List(CU.temp(stage(4), tr742), Const("96i"), Const("0i")), op=Mux, results=List(CU.temp(stage(5), tr743)))
      Stage(stage(6), operands=List(CU.temp(stage(5), tr740), CU.temp(stage(5), tr743)), op=FixAdd, results=List(CU.scalarOut(stage(6), x4741_mc.len)))
    }
    val x4807 = UnitPipeline(name = "x4807", parent=x5038, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val x4807_unitcc = CounterChain(name = "x4807_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      var stage: List[Stage] = Nil
    }
    val x4782 = StreamController(name = "x4782", parent=x4807, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val x4713 = CounterChain.copy(x5038, "x4713")
      val x4782_unitcc = CounterChain(name = "x4782_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      var stage: List[Stage] = Nil
    }
    val x4782_0 = StreamPipeline(name = "x4782_0", parent=x4782, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val x4713 = CounterChain.copy(x5038, "x4713")
      val x4782_unitcc = CounterChain.copy(x4782, "x4782_unitcc")
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(1)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4713(0)), Const("96i")), op=FixMod, results=List(CU.scalarOut(stage(1), x4764_scalar), CU.scalarOut(stage(1), bus_758_scalar)))
    }
    val x4782_1 = StreamPipeline(name = "x4782_1", parent=x4782, deps=List(x4782_0)) { implicit CU => 
      val stage0 = CU.emptyStage
      val x4713 = CounterChain.copy(x5038, "x4713")
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(1)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4713(0)), CU.scalarIn(stage(0), bus_758_scalar)), op=FixSub, results=List(CU.scalarOut(stage(1), x4786_mc.ofs)))
    }
    val x4782_2 = StreamPipeline(name = "x4782_2", parent=x4782, deps=List(x4782_0)) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr768 = CU.temp
      val tr767 = CU.temp
      val tr765 = CU.temp
      val tr764 = CU.temp
      val tr762 = CU.temp
      val x4713 = CounterChain.copy(x5038, "x4713")
      val x4782_unitcc = CounterChain.copy(x4782, "x4782_unitcc")
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(6)
      Stage(stage(1), operands=List(CU.scalarIn(stage(0), bus_758_scalar), Const("96i")), op=FixAdd, results=List(CU.scalarOut(stage(1), x4765_scalar), CU.temp(stage(1), tr762)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr762), Const("96i")), op=FixMod, results=List(CU.temp(stage(2), tr764)))
      Stage(stage(3), operands=List(CU.temp(stage(2), tr762), CU.temp(stage(2), tr764)), op=FixSub, results=List(CU.temp(stage(3), tr765)))
      Stage(stage(4), operands=List(CU.temp(stage(3), tr764), Const("0i")), op=FixNeq, results=List(CU.temp(stage(4), tr767)))
      Stage(stage(5), operands=List(CU.temp(stage(4), tr767), Const("96i"), Const("0i")), op=Mux, results=List(CU.temp(stage(5), tr768)))
      Stage(stage(6), operands=List(CU.temp(stage(5), tr765), CU.temp(stage(5), tr768)), op=FixAdd, results=List(CU.scalarOut(stage(6), x4786_mc.len)))
    }
    val x5036 = MetaPipeline(name = "x5036", parent=x5038, deps=List(x4762, x4807)) { implicit CU => 
      val stage0 = CU.emptyStage
      val ctr12 = (Const("0i").out, Const("96i").out, Const("1i").out) // Counter
      val x4811 = CounterChain(name = "x4811", ctr12)
      val ctr14 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4813 = CounterChain(name = "x4813", ctr14)
      var stage: List[Stage] = Nil
    }
    val x4847_0 = Pipeline(name = "x4847_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr784 = CU.temp
      val ctr15 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4830 = CounterChain(name = "x4830", ctr15)
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4709 = CounterChain.copy(x5058, "x4709")
      val x4702_x4839 = SRAM(size = 192, writeCtr = x4709(0), banking = Duplicated(), buffering = SingleBuffer()).wtPort(x4702_vector).rdAddr(x4830(0)).wtAddr(x4709(0))
      val x4714_x4836 = SemiFIFO(size = 18432, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4741_mc.dataIn).wtStart(CU.scalarIn(stage0, x4719_scalar).out).wtEnd(CU.scalarIn(stage0, x4720_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(4)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4811(0)), Const("192i")), op=FixMul, results=List(CU.temp(stage(1), tr784)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr784), CU.ctr(stage(1), x4830(0))), op=FixAdd, results=List(x4714_x4836.readAddr))
      Stage(stage(3), operands=List(x4714_x4836.load, CU.load(stage(2), x4702_x4839)), op=FltMul, results=List(CU.reduce(stage(3))))
      val (rs1, rr793) = Stage.reduce(op=FltAdd, init=Const("0i"))
      Stage(stage(4), operands=List(rr793), op=Bypass, results=List(CU.scalarOut(stage(4), x4822_scalar)))
    }
    val x4861_0 = Pipeline(name = "x4861_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr798 = CU.temp
      val ctr16 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4831 = CounterChain(name = "x4831", ctr16)
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4709 = CounterChain.copy(x5058, "x4709")
      val x4702_x4853 = SRAM(size = 192, writeCtr = x4709(0), banking = Duplicated(), buffering = SingleBuffer()).wtPort(x4702_vector).rdAddr(x4831(0)).wtAddr(x4709(0))
      val x4714_x4850 = SemiFIFO(size = 18432, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4741_mc.dataIn).wtStart(CU.scalarIn(stage0, x4719_scalar).out).wtEnd(CU.scalarIn(stage0, x4720_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(4)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4811(0)), Const("192i")), op=FixMul, results=List(CU.temp(stage(1), tr798)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr798), CU.ctr(stage(1), x4831(0))), op=FixAdd, results=List(x4714_x4850.readAddr))
      Stage(stage(3), operands=List(x4714_x4850.load, CU.load(stage(2), x4702_x4853)), op=FltMul, results=List(CU.reduce(stage(3))))
      val (rs1, rr807) = Stage.reduce(op=FltAdd, init=Const("0i"))
      Stage(stage(4), operands=List(rr807), op=Bypass, results=List(CU.scalarOut(stage(4), x4823_scalar)))
    }
    val x4875_0 = Pipeline(name = "x4875_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr812 = CU.temp
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4709 = CounterChain.copy(x5058, "x4709")
      val ctr17 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4832 = CounterChain(name = "x4832", ctr17)
      val x4714_x4864 = SemiFIFO(size = 18432, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4741_mc.dataIn).wtStart(CU.scalarIn(stage0, x4719_scalar).out).wtEnd(CU.scalarIn(stage0, x4720_scalar).out)
      val x4702_x4867 = SRAM(size = 192, writeCtr = x4709(0), banking = Duplicated(), buffering = SingleBuffer()).wtPort(x4702_vector).rdAddr(x4832(0)).wtAddr(x4709(0))
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(4)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4811(0)), Const("192i")), op=FixMul, results=List(CU.temp(stage(1), tr812)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr812), CU.ctr(stage(1), x4832(0))), op=FixAdd, results=List(x4714_x4864.readAddr))
      Stage(stage(3), operands=List(x4714_x4864.load, CU.load(stage(2), x4702_x4867)), op=FltMul, results=List(CU.reduce(stage(3))))
      val (rs1, rr821) = Stage.reduce(op=FltAdd, init=Const("0i"))
      Stage(stage(4), operands=List(rr821), op=Bypass, results=List(CU.scalarOut(stage(4), x4824_scalar)))
    }
    val x4889_0 = Pipeline(name = "x4889_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr826 = CU.temp
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4709 = CounterChain.copy(x5058, "x4709")
      val ctr18 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4833 = CounterChain(name = "x4833", ctr18)
      val x4702_x4881 = SRAM(size = 192, writeCtr = x4709(0), banking = Duplicated(), buffering = SingleBuffer()).wtPort(x4702_vector).rdAddr(x4833(0)).wtAddr(x4709(0))
      val x4714_x4878 = SemiFIFO(size = 18432, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4741_mc.dataIn).wtStart(CU.scalarIn(stage0, x4719_scalar).out).wtEnd(CU.scalarIn(stage0, x4720_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(4)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4811(0)), Const("192i")), op=FixMul, results=List(CU.temp(stage(1), tr826)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr826), CU.ctr(stage(1), x4833(0))), op=FixAdd, results=List(x4714_x4878.readAddr))
      Stage(stage(3), operands=List(x4714_x4878.load, CU.load(stage(2), x4702_x4881)), op=FltMul, results=List(CU.reduce(stage(3))))
      val (rs1, rr835) = Stage.reduce(op=FltAdd, init=Const("0i"))
      Stage(stage(4), operands=List(rr835), op=Bypass, results=List(CU.scalarOut(stage(4), x4825_scalar)))
    }
    val x4902_0 = UnitPipeline(name = "x4902_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr847 = CU.temp
      val tr846 = CU.temp
      val tr844 = CU.temp
      val tr843 = CU.temp
      val x4902_unitcc = CounterChain(name = "x4902_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4715_x4893 = SemiFIFO(size = 96, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4786_mc.dataIn).rdAddr(x4811(0)).wtStart(CU.scalarIn(stage0, x4764_scalar).out).wtEnd(CU.scalarIn(stage0, x4765_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(5)
      Stage(stage(1), operands=List(Const("-1.0f"), CU.scalarIn(stage(0), x4822_scalar)), op=FltMul, results=List(CU.temp(stage(1), tr843)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr843)), op=FltExp, results=List(CU.temp(stage(2), tr844)))
      Stage(stage(3), operands=List(CU.temp(stage(2), tr844), Const("1i")), op=FltAdd, results=List(CU.temp(stage(3), tr846)))
      Stage(stage(4), operands=List(Const("1i"), CU.temp(stage(3), tr846)), op=FltDiv, results=List(CU.temp(stage(4), tr847)))
      Stage(stage(5), operands=List(CU.load(stage(4), x4715_x4893), CU.temp(stage(4), tr847)), op=FltSub, results=List(CU.scalarOut(stage(5), x4814_scalar)))
    }
    val x4913_0 = UnitPipeline(name = "x4913_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr860 = CU.temp
      val tr859 = CU.temp
      val tr857 = CU.temp
      val tr856 = CU.temp
      val x4913_unitcc = CounterChain(name = "x4913_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4715_x4904 = SemiFIFO(size = 96, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4786_mc.dataIn).rdAddr(x4811(0)).wtStart(CU.scalarIn(stage0, x4764_scalar).out).wtEnd(CU.scalarIn(stage0, x4765_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(5)
      Stage(stage(1), operands=List(Const("-1.0f"), CU.scalarIn(stage(0), x4823_scalar)), op=FltMul, results=List(CU.temp(stage(1), tr856)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr856)), op=FltExp, results=List(CU.temp(stage(2), tr857)))
      Stage(stage(3), operands=List(CU.temp(stage(2), tr857), Const("1i")), op=FltAdd, results=List(CU.temp(stage(3), tr859)))
      Stage(stage(4), operands=List(Const("1i"), CU.temp(stage(3), tr859)), op=FltDiv, results=List(CU.temp(stage(4), tr860)))
      Stage(stage(5), operands=List(CU.load(stage(4), x4715_x4904), CU.temp(stage(4), tr860)), op=FltSub, results=List(CU.scalarOut(stage(5), x4815_scalar)))
    }
    val x4924_0 = UnitPipeline(name = "x4924_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr873 = CU.temp
      val tr872 = CU.temp
      val tr870 = CU.temp
      val tr869 = CU.temp
      val x4924_unitcc = CounterChain(name = "x4924_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4715_x4915 = SemiFIFO(size = 96, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4786_mc.dataIn).rdAddr(x4811(0)).wtStart(CU.scalarIn(stage0, x4764_scalar).out).wtEnd(CU.scalarIn(stage0, x4765_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(5)
      Stage(stage(1), operands=List(Const("-1.0f"), CU.scalarIn(stage(0), x4824_scalar)), op=FltMul, results=List(CU.temp(stage(1), tr869)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr869)), op=FltExp, results=List(CU.temp(stage(2), tr870)))
      Stage(stage(3), operands=List(CU.temp(stage(2), tr870), Const("1i")), op=FltAdd, results=List(CU.temp(stage(3), tr872)))
      Stage(stage(4), operands=List(Const("1i"), CU.temp(stage(3), tr872)), op=FltDiv, results=List(CU.temp(stage(4), tr873)))
      Stage(stage(5), operands=List(CU.load(stage(4), x4715_x4915), CU.temp(stage(4), tr873)), op=FltSub, results=List(CU.scalarOut(stage(5), x4816_scalar)))
    }
    val x4935_0 = UnitPipeline(name = "x4935_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr886 = CU.temp
      val tr885 = CU.temp
      val tr883 = CU.temp
      val tr882 = CU.temp
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4935_unitcc = CounterChain(name = "x4935_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      val x4715_x4926 = SemiFIFO(size = 96, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4786_mc.dataIn).rdAddr(x4811(0)).wtStart(CU.scalarIn(stage0, x4764_scalar).out).wtEnd(CU.scalarIn(stage0, x4765_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(5)
      Stage(stage(1), operands=List(Const("-1.0f"), CU.scalarIn(stage(0), x4825_scalar)), op=FltMul, results=List(CU.temp(stage(1), tr882)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr882)), op=FltExp, results=List(CU.temp(stage(2), tr883)))
      Stage(stage(3), operands=List(CU.temp(stage(2), tr883), Const("1i")), op=FltAdd, results=List(CU.temp(stage(3), tr885)))
      Stage(stage(4), operands=List(Const("1i"), CU.temp(stage(3), tr885)), op=FltDiv, results=List(CU.temp(stage(4), tr886)))
      Stage(stage(5), operands=List(CU.load(stage(4), x4715_x4926), CU.temp(stage(4), tr886)), op=FltSub, results=List(CU.scalarOut(stage(5), x4817_scalar)))
    }
    val x4958_0 = Pipeline(name = "x4958_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr892 = CU.temp
      val ctr19 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4942 = CounterChain(name = "x4942", ctr19)
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4714_x4948 = SemiFIFO(size = 18432, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4741_mc.dataIn).wtStart(CU.scalarIn(stage0, x4719_scalar).out).wtEnd(CU.scalarIn(stage0, x4720_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(3)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4811(0)), Const("192i")), op=FixMul, results=List(CU.temp(stage(1), tr892)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr892), CU.ctr(stage(1), x4942(0))), op=FixAdd, results=List(x4714_x4948.readAddr))
      Stage(stage(3), operands=List(x4714_x4948.load, CU.scalarIn(stage(2), x4814_scalar)), op=FltSub, results=List(CU.vecOut(stage(3), x4818_vector)))
    }
    val x4971_0 = Pipeline(name = "x4971_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr905 = CU.temp
      val ctr20 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4943 = CounterChain(name = "x4943", ctr20)
      val x4811 = CounterChain.copy(x5036, "x4811")
      val x4714_x4961 = SemiFIFO(size = 18432, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4741_mc.dataIn).wtStart(CU.scalarIn(stage0, x4719_scalar).out).wtEnd(CU.scalarIn(stage0, x4720_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(3)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4811(0)), Const("192i")), op=FixMul, results=List(CU.temp(stage(1), tr905)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr905), CU.ctr(stage(1), x4943(0))), op=FixAdd, results=List(x4714_x4961.readAddr))
      Stage(stage(3), operands=List(x4714_x4961.load, CU.scalarIn(stage(2), x4815_scalar)), op=FltSub, results=List(CU.vecOut(stage(3), x4819_vector)))
    }
    val x4984_0 = Pipeline(name = "x4984_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr918 = CU.temp
      val x4811 = CounterChain.copy(x5036, "x4811")
      val ctr21 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4944 = CounterChain(name = "x4944", ctr21)
      val x4714_x4974 = SemiFIFO(size = 18432, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4741_mc.dataIn).wtStart(CU.scalarIn(stage0, x4719_scalar).out).wtEnd(CU.scalarIn(stage0, x4720_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(3)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4811(0)), Const("192i")), op=FixMul, results=List(CU.temp(stage(1), tr918)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr918), CU.ctr(stage(1), x4944(0))), op=FixAdd, results=List(x4714_x4974.readAddr))
      Stage(stage(3), operands=List(x4714_x4974.load, CU.scalarIn(stage(2), x4816_scalar)), op=FltSub, results=List(CU.vecOut(stage(3), x4820_vector)))
    }
    val x4997_0 = Pipeline(name = "x4997_0", parent=x5036, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr931 = CU.temp
      val x4811 = CounterChain.copy(x5036, "x4811")
      val ctr22 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4945 = CounterChain(name = "x4945", ctr22)
      val x4714_x4987 = SemiFIFO(size = 18432, banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4811(0))).wtPort(x4741_mc.dataIn).wtStart(CU.scalarIn(stage0, x4719_scalar).out).wtEnd(CU.scalarIn(stage0, x4720_scalar).out)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(3)
      Stage(stage(1), operands=List(CU.ctr(stage(0), x4811(0)), Const("192i")), op=FixMul, results=List(CU.temp(stage(1), tr931)))
      Stage(stage(2), operands=List(CU.temp(stage(1), tr931), CU.ctr(stage(1), x4945(0))), op=FixAdd, results=List(x4714_x4987.readAddr))
      Stage(stage(3), operands=List(x4714_x4987.load, CU.scalarIn(stage(2), x4817_scalar)), op=FltSub, results=List(CU.vecOut(stage(3), x4821_vector)))
    }
    val x5034 = StreamController(name = "x5034", parent=x5036, deps=List(x4958_0, x4971_0, x4984_0, x4997_0)) { implicit CU => 
      val stage0 = CU.emptyStage
      val x4942 = CounterChain.copy(x4958_0, "x4942")
      val ctr25 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4813 = CounterChain(name = "x4813", ctr25)
      val x4943 = CounterChain.copy(x4971_0, "x4943")
      val x4945 = CounterChain.copy(x4997_0, "x4945")
      val x4944 = CounterChain.copy(x4984_0, "x4944")
      var stage: List[Stage] = Nil
    }
    val x5034_0 = StreamPipeline(name = "x5034_0", parent=x5034, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr972 = CU.temp
      val tr971 = CU.temp
      val tr968 = CU.temp
      val tr965 = CU.temp
      val tr964 = CU.temp
      val tr961 = CU.temp
      val x4942 = CounterChain.copy(x4958_0, "x4942")
      val x4943 = CounterChain.copy(x4971_0, "x4943")
      val x4945 = CounterChain.copy(x4997_0, "x4945")
      val x4813 = CounterChain.copy(x5034, "x4813")
      val x4944 = CounterChain.copy(x4984_0, "x4944")
      val x4819_x5005 = SRAM(size = 192, writeCtr = x4943(0), banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4813(0), swapWrite = x4943(0))).wtPort(x4819_vector).rdAddr(x4813(0)).wtAddr(x4943(0))
      val x4821_x5011 = SRAM(size = 192, writeCtr = x4945(0), banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4813(0), swapWrite = x4945(0))).wtPort(x4821_vector).rdAddr(x4813(0)).wtAddr(x4945(0))
      val x4818_x5002 = SRAM(size = 192, writeCtr = x4942(0), banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4813(0), swapWrite = x4942(0))).wtPort(x4818_vector).rdAddr(x4813(0)).wtAddr(x4942(0))
      val x4820_x5008 = SRAM(size = 192, writeCtr = x4944(0), banking = Duplicated(), buffering = MultiBuffer(2, swapRead = x4813(0), swapWrite = x4944(0))).wtPort(x4820_vector).rdAddr(x4813(0)).wtAddr(x4944(0))
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(7)
      Stage(stage(1), operands=List(CU.ctrl(stage(0), cr959), x4818_x5002.load, Const("0i")), op=Mux, results=List(CU.temp(stage(1), tr961)))
      Stage(stage(2), operands=List(CU.ctrl(stage(1), cr963), CU.load(stage(1), x4819_x5005), Const("0i")), op=Mux, results=List(CU.temp(stage(2), tr964)))
      Stage(stage(3), operands=List(CU.temp(stage(2), tr961), CU.temp(stage(2), tr964)), op=FltAdd, results=List(CU.temp(stage(3), tr965)))
      Stage(stage(4), operands=List(CU.ctrl(stage(3), cr967), CU.load(stage(3), x4820_x5008), Const("0i")), op=Mux, results=List(CU.temp(stage(4), tr968)))
      Stage(stage(5), operands=List(CU.ctrl(stage(4), cr970), CU.load(stage(4), x4821_x5011), Const("0i")), op=Mux, results=List(CU.temp(stage(5), tr971)))
      Stage(stage(6), operands=List(CU.temp(stage(5), tr968), CU.temp(stage(5), tr971)), op=FltAdd, results=List(CU.temp(stage(6), tr972)))
      Stage(stage(7), operands=List(CU.temp(stage(6), tr965), CU.temp(stage(6), tr972)), op=FltAdd, results=List(CU.vecOut(stage(7), bus_973_vector)))
    }
    val x5034_1 = StreamPipeline(name = "x5034_1", parent=x5034, deps=List(x5034_0)) { implicit CU => 
      val stage0 = CU.emptyStage
      val x4942 = CounterChain.copy(x4958_0, "x4942")
      val x4943 = CounterChain.copy(x4971_0, "x4943")
      val x4945 = CounterChain.copy(x4997_0, "x4945")
      val x4813 = CounterChain.copy(x5034, "x4813")
      val x4944 = CounterChain.copy(x4984_0, "x4944")
      val x4710_x5014 = SRAM(size = 192, writeCtr = x4813(0), banking = Duplicated(), buffering = SingleBuffer()).rdAddr(x4813(0))
      val bus_973_fifo = FIFO(size = 4096, banking = Strided(1)).wtPort(bus_973_vector)
      val wr975 = CU.wtAddr(x4710_x5014)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(2)
      Stage(stage(1), operands=List(bus_973_fifo.load, x4710_x5014.load), op=FltAdd, results=List(CU.vecOut(stage(1), x4710_vector), CU.store(stage(1), x4710_x5014)))
      Stage(stage(2), operands=List(CU.ctr(stage(1), x4813(0))), op=Bypass, results=List(CU.wtAddr(stage(2), wr975)))
    }
    val x5056_0 = Pipeline(name = "x5056_0", parent=x5058, deps=List(x5038)) { implicit CU => 
      val stage0 = CU.emptyStage
      val tr989 = CU.temp
      val tr987 = CU.temp
      val x4813 = CounterChain.copy(x5034, "x4813")
      val x4709 = CounterChain.copy(x5058, "x4709")
      val ctr26 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x4709 = CounterChain(name = "x4709", ctr26)
      val x4710_x5041 = SRAM(size = 192, writeCtr = x4813(0), banking = Duplicated(), buffering = SingleBuffer()).wtPort(x4710_vector).rdAddr(x4709(0)).wtAddr(x4813(0))
      val x4702_x5044 = SRAM(size = 192, writeCtr = x4709(0), banking = Duplicated(), buffering = SingleBuffer()).rdAddr(x4709(0))
      val wr991 = CU.wtAddr(x4702_x5044)
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(4)
      Stage(stage(1), operands=List(CU.ctrl(stage(0), cr985), x4710_x5041.load, Const("0i")), op=Mux, results=List(CU.temp(stage(1), tr987)))
      Stage(stage(2), operands=List(CU.load(stage(1), x4702_x5044), Const("1i")), op=FltMul, results=List(CU.temp(stage(2), tr989)))
      Stage(stage(3), operands=List(CU.temp(stage(2), tr987), CU.temp(stage(2), tr989)), op=FltAdd, results=List(CU.vecOut(stage(3), x4702_vector), CU.store(stage(3), x4702_x5044)))
      Stage(stage(4), operands=List(CU.ctr(stage(3), x4709(0))), op=Bypass, results=List(CU.wtAddr(stage(4), wr991)))
    }
    val x5080 = StreamController(name = "x5080", parent=x5082, deps=List(x5060)) { implicit CU => 
      val stage0 = CU.emptyStage
      val x5080_unitcc = CounterChain(name = "x5080_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      var stage: List[Stage] = Nil
    }
    val x5065_0 = UnitPipeline(name = "x5065_0", parent=x5080, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val x5065_unitcc = CounterChain(name = "x5065_unitcc", (Const("0i"), Const("1i"), Const("1i")))
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(2)
      Stage(stage(1), operands=List(Const("0i")), op=Bypass, results=List(CU.scalarOut(stage(1), x5078_mc.ofs)))
      Stage(stage(2), operands=List(Const("192i")), op=Bypass, results=List(CU.scalarOut(stage(2), x5078_mc.len)))
    }
    val x5076_0 = Pipeline(name = "x5076_0", parent=x5080, deps=List()) { implicit CU => 
      val stage0 = CU.emptyStage
      val x4709 = CounterChain.copy(x5058, "x4709")
      val ctr27 = (Const("0i").out, Const("192i").out, Const("1i").out) // Counter
      val x5067 = CounterChain(name = "x5067", ctr27)
      val x4702_x5070 = SRAM(size = 192, writeCtr = x4709(0), banking = Duplicated(), buffering = SingleBuffer()).wtPort(x4702_vector).rdAddr(x5067(0)).wtAddr(x4709(0))
      var stage: List[Stage] = Nil
      stage = stage0 +: Stages(1)
      Stage(stage(1), operands=List(x4702_x5070.load), op=Bypass, results=List(CU.vecOut(stage(1), x5078_mc.dataOut)))
    }
    
  }
}
