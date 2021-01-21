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
val nget = getInt();
val linel = Array.array(100000,0);
val lineh = Array.array(100000,0);
val liner = Array.array(100000,0);
val cnt = Array.array(1,0);
val first = Array.array(100000,0x3fffffff);
val last = Array.array(100000,0);
val MAX = Array.array(100000,0);
val mmmmax = Array.array(1,0)
val nn = Array.array(1,~1)
fun read(x) = 
let
  val cn = Array.sub(cnt,0)
  val kkk = Array.update(cnt,0,cn+1)
  val ll = getInt() - 1
  val l = Array.update(linel,cn,ll)
  val h = Array.update(lineh,cn,getInt())
  val rr= getInt() -1
  val mmax = Array.sub(mmmmax,0)
  val upmax = if(rr>mmax) then let val fuck = Array.update(mmmmax,0,rr) in 0 end
              else 0
  val r = Array.update(liner,cn,rr)
  val n = Array.sub(nn,0) + 1
  val upfirst = Array.update(first,n,ll)
  val upfirst = Array.update(first,n+1,rr)
  val nnn = Array.update(nn,0,n+1)
in 0 end;
val rea = List.tabulate(nget,read);
val ccnt = Array.sub(cnt,0) - 1;
val upcnt = Array.update(cnt,0,ccnt);
fun arrayToList arr = Array.foldr (op ::) [] arr;
val ffirst = arrayToList(first);
fun quicksort(lst) = 
if(length(lst) = 0) then lst
else let
	val p = List.nth(lst,0)
	val s1 = List.filter(fn x => x < p) lst
	val s2 = [p]
	val s3 = List.filter(fn x => x > p) lst
	val ss1 = quicksort(s1)
	val ss2 = quicksort(s3)
in
	ss1 @ s2 @ ss2
end;
val ffirst = quicksort(ffirst);
val last = Array.fromList ffirst;
val upn = Array.update(nn,0,length(ffirst));
fun Map(x,l,r) = if(l>r) then ~1 else 
let
  val mid = (l+r) div 2
  val k = printInt(mid)
  val lstmid = Array.sub(last,mid)
in 
  if(lstmid = x) then mid else if(lstmid > x) then Map(x,l,mid) else
    Map(x,mid+1,r)
end 
val n = Array.sub(nn,0);
fun build _ = 
let
  val ccnt = Array.sub(cnt,0)
  val foeach = List.tabulate(ccnt+1,fn i =>
  let
    val l = Array.sub(linel,i)
    val r = Array.sub(liner,i)
    
    val foveach = List.tabulate(r,fn j => if(j<l) then 0 else
      let
        
        val mj = Array.sub(MAX,j)
        val h = Array.sub(lineh,i)
        val upj = Array.update(MAX,j,Int.max(mj,h))
      in 0 end
    )  
  in 0 end)
  in 0 end;
build 0;
MAX;
fun printtt(s,ccnt,pre) = if(s > ccnt) then 0
                          else
                            let
                              val qus = Array.sub(MAX,s)
            val pr = if(qus <> pre) then
              let
                val m = printInt(s+1)
                val mm = printInt(qus)
                val mmm = printEndOfLine()
                in qus end 
                                       else pre
                            in printtt(s+1,ccnt,pr) end;
val target = Array.sub(mmmmax,0);
printtt(0,target,0);
(*****End*****)







