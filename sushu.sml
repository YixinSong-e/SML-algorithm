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
val lst:IntInf.int list = [2,3,5,7,11,13,17,19,23,29]
val prime = Array.fromList lst;
val tt = [0];
val t = Array.fromList tt;
fun quickmin(x:IntInf.int, y:IntInf.int, p:IntInf.int) = 
    if(y = 0) then 1
    else 
        let
            val ans:IntInf.int = quickmin(x, y div 2, p);
            fun c() = if(y mod 2 = 0) then  (ans*ans) mod p else (ans*ans) mod p * x mod p;
        in
            c()
        end
fun split(x:IntInf.int,num) = if(x mod 2 = 1) then (num,x) else split(x div
  2,num+1);
fun miller(x:IntInf.int):bool = 
let
  val s = 10
  val u:IntInf.int = x - 1
  val ifff= if( x = 2) then true else if( x<2 orelse x mod 2 = 0 ) then false else
    let
      val (nn,u) = split(u,0)
      val fk = Array.update(t,0,nn)
      val judge = List.tabulate(s,fn i => let val a = Array.sub(prime,i)
                                          in if(a>=x) then true  else
      let
        val a = Array.sub(prime,i)
        val b = quickmin(a,u,x)
        val bb = Array.array(1,b)
        val ttt = Array.sub(t,0)
        val preju = Array.array(1,1)
        val jju = List.tabulate(ttt,fn j =>
        let
          val b = Array.sub(bb,0)
          val y = (b*b mod x)
          val jjud = if( y=1 andalso b <>1 andalso b <> x-1) then 
            let
              val kkkkk = Array.update(preju,0,0)
            in 0 end else 0
          val upb = Array.update(bb,0,y)

        in 0 end)
        val getval = Array.sub(preju,0)
        val buer = if(getval = 0) then false else 
          let
            val b = Array.sub(bb,0)
          in
            if(b<>1) then false else true
          end
      in buer end
                                          end)
     fun findd(s,now) = if(now = s) then true else let val value = List.nth(judge,now) in
       if(value = false) then false else findd(s,now+1) end
      val boolean = if(findd(s,0) = false) then false else true 
    in boolean end
in ifff end;
miller(101);
miller(34);
miller(7297);
(*****End*****)








