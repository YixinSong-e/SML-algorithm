val input = TextIO.openIn("input.txt");

fun printInt (a:int) =
    print(Int.toString(a)^" ");

fun printIntInf (a:IntInf.int) =
    print(IntInf.toString(a)^" ");


fun printReal (a:real) =
    print(Real.toString(a)^" ");

fun printString (a:string) =
    print(a^" ");

fun getInt () =
    Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input);

fun getIntInf () =
    Option.valOf (TextIO.scanStream (IntInf.scan StringCvt.DEC) input);

fun getReal () =
    Option.valOf (TextIO.scanStream (Real.scan) input);

fun printEndOfLine () =
    print("\n");

fun printIntTable ( [] ) = ()
  | printIntTable ( x::xs ) = 
    let
	val tmp = printInt(x)
    in
	printIntTable(xs)
    end;

fun printIntInfTable ( [] ) = ()
  | printIntInfTable ( x::xs ) = 
    let
	val tmp = printIntInf(x)
    in
	printIntInfTable(xs)
    end;

fun getIntTable ( 0 ) = []
  | getIntTable ( N:int) = getInt()::getIntTable(N-1);

fun getIntInfTable ( 0 ) = []
  | getIntInfTable ( N:int) = getIntInf()::getIntInfTable(N-1);

fun getIntVector ( 0 ) =  Vector.fromList []
  | getIntVector ( N:int) = Vector.fromList(getIntTable(N));

fun getIntInfVector ( 0 ) = Vector.fromList []
  | getIntInfVector ( N:int) = Vector.fromList(getIntInfTable(N));


(*****Begin*****)
val n = getInt();
val m = getInt();
val edge = Array2.array(n,n,0);
fun rea(x) = 
let
  val a = getInt() - 1;
  val b = getInt() - 1;
  val k = Array2.update(edge,a,b,1);
  val k = Array2.update(edge,b,a,1);
in 0 end;
val k = List.tabulate(m,rea);
val dfn = Array.array(n,0);
val low = Array.array(n,0);
val bridge = Array.array(1,0);
val tot = Array.array(1,0);
val cut = Array.array(n,0);
fun tarjan(u,fa) = 
let
  val child = Array.array(1,0)
  val t = Array.sub(tot,0)
  val upt = Array.update(tot,0,t+1)
  val du = Array.update(dfn,u,t+1)
  val lu = Array.update(low,u,t+1)
  val foreach = List.tabulate(n,fn v=>
  let
     val p = Array2.sub(edge,u,v)
    val ifexist = if( p = 0) then 0 else if(v=fa) then 0
                  else
                    let
                      val dv = Array.sub(dfn,v)
                      val ifv= if(dv =0) then let
                        val tar = tarjan(v,u)
                        val lu = Array.sub(low,u)
                        val lv = Array.sub(low,v)
                        val upu = Array.update(low,u,Int.min(lu,lv))
                        val c = Array.sub(child,0)
                        val bri = Array.sub(bridge,0)
                        val du = Array.sub(dfn,u)
                        val kk = if(lv > du) then let val mmmm =
                                  Array.update(bridge,0,bri+1) in 0 end else 0

                        val kkk = if(u=fa) then let val cc =
                          Array.update(child,0,c+1) in 0 end else 
                            let val lv = Array.sub(low,v)
                                val du = Array.sub(dfn,u)
                                val k = if(lv>=du) then let val m= Array.update(cut,u,1)
                                        in 0 end else 0
                            in 0 end 
                                              in 0 end
                               else
                                 let
                                   val lu = Array.sub(low,u)
                                   val dv = Array.sub(dfn,v)
                                   val upu = Array.update(low,u,Int.min(lu,dv))
                                 in 0 end
                    in 0 end
  in 0 end
  )

  val cc = Array.sub(child,0)
  val laststep = if(u = fa andalso cc >=2) then let val m =
    Array.update(cut,u,1) in 0 end else 0
in 0 end;
fun rrun(x) = let
  val k = Array.sub(dfn,x)
in
  if(k = 0) then tarjan(x,x) else 0 
end;
val zz = List.tabulate(n,rrun);
printInt (Array.foldl (fn (x, y) => if x <> 0 then y + 1 else y) 0 cut);
bridge;
(*****End*****)






