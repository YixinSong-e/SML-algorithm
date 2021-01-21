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
val t = getInt();
val edge = Array2.array(n,n,0x3ffffff);
val dist = Array.array(n,0x3ffffff);
val vst = Array.array(n,0);
val ss = Array.update(dist,t-1,0);
fun inpu(x) = 
let 
	val a = getInt() -1;
	val b = getInt() -1;
	val c = getInt();
	val k = Array2.sub(edge,a,b);
        val minn = Int.min(c,k);
	val ss = Array2.update(edge,a,b,minn);
	val ss = Array2.update(edge,b,a,minn);
in 0 end;
if(m = 0) then 0
else let val ss = List.tabulate(m,inpu) in 0 end;
fun findshortest (minn,s,len,node) = if(s = len) then node
else let
	val short = Array.sub(dist,s)
	val vis = Array.sub(vst,s)
	val k = if(short < minn andalso vis = 0) then 1 else 0
	val kk = if(k = 1) then short else minn
	val nn = if(k = 1) then s else node
in  findshortest(kk,s+1,len,nn) end;
fun dijkstra(x) = 
let
  val no = findshortest(0x3fffffff,0,n,t)
  val ss = Array.update(vst,no,1)
  val dis = Array.sub(dist,no)
  val ss = List.tabulate(n,fn y => 
  let
    val k = Array2.sub(edge,no,y)
    val diss = Array.sub(dist,y)
    val sss = if(diss > k + dis) then let val ssss = Array.update(dist,y,k+dis)
    in y+dis end else diss
  in sss end)
in dis end;
val an = List.tabulate(n,dijkstra);
fun convert (x) = 
let
  val ss = Array.sub(dist,x)
  val k = if(ss = 0x3ffffff) then let val kkk = Array.update(dist,x,~1) in 0 end
          else 0
in 0 end;
val kk = List.tabulate(n,convert);
fun arrayToList arr = Array.foldr (op ::) [] arr;
val lstt = arrayToList(dist);

(*****End*****)



