module Exam.ThirdQuestion

let calculatePi (x: uint64) : decimal =
    let nCalc sign n =
        (sign * 4.0M) / decimal (n * (n+1M) * (n+2M))

    let rec aux (current: decimal) (sign: decimal) (acc: decimal) : decimal =
        match current with
        | n when n > (decimal x) -> acc
        | _ -> aux (current + 1M) (sign * -1.0M) (acc + (nCalc sign (current * 2M)))

    aux 1M 1.0M 3M

let piSeq : seq<decimal> =
    seq {
        yield 3.0M;

        let rec aux (current: uint64) : seq<decimal> =
            seq {
                yield (calculatePi current);

                yield! aux (current + 1UL)
            }

        yield! aux 1UL
    }

// Better solution
let piSeq2 = Seq.initInfinite (uint64 >> calculatePi)

let circleArea (r: float) : seq<decimal> =
    let r = decimal r * decimal r
    seq {
        for i in piSeq do
            yield (i * r);
    }

// Better solution
let caAux r pi = pi * r * r
let circleArea2 r = Seq.map (caAux (decimal r)) piSeq

let sphereVolume (r: float) : seq<decimal> =
    let r = decimal r * decimal r * decimal r

    seq {
        for i in piSeq do
            yield (4M/3M * i * r);
    }

// Better solution
let svAux r pi = pi * (4M/3M) * r * r * r
let sphereVolume2 r = Seq.map (svAux (decimal r)) piSeq

let circleSphere (r: float) : seq<decimal * decimal> =
    Seq.map2 (fun x y -> (x, y)) (circleArea r) (sphereVolume r)

// Real solution
let circleSphere2 (radius : float) = 
    let r = decimal radius
    seq { for pi in piSeq do yield (caAux r pi, svAux r pi) }


let parallelPi (numberOfProcesses: uint64) (iterationsPerProcess: uint64) : decimal =
    let getSign n =
        match n % 2M with
        | 0M -> -1M
        | _ -> 1M

    let rec calc ns acc=
        match ns with
        | [] -> acc
        | n::ns' ->
            calc ns' (acc + ((getSign n) * 4.0M) / (n * 2M * (n * 2M + 1M) * (n * 2M + 2M)))
        

    let nCalc ns =
        async {
            return calc (ns |> Seq.toList) 0M
        }

    (seq { 1M .. decimal (numberOfProcesses * iterationsPerProcess) }
    |> Seq.splitInto (int numberOfProcesses)
    |> Seq.map nCalc
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.sum) + 3M

// printfn $"Pi: %M{calculatePi 10UL}" - Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
// printfn $"Pi: %M{parallelPi 2UL 5UL}" - Real: 00:00:00.003, CPU: 00:00:00.020, GC gen0: 0, gen1: 0, gen2: 0

// printfn $"Pi: %M{calculatePi 20UL}" - Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
// printfn $"Pi: %M{parallelPi 4UL 5UL}" - Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
// printfn $"Pi: %M{parallelPi 2UL 10UL}" - Real: 00:00:00.000, CPU: 00:00:00.009, GC gen0: 0, gen1: 0, gen2: 0

// printfn $"Pi: %M{parallelPi 10UL 1000000UL}" - Real: 00:00:03.744, CPU: 00:00:06.130, GC gen0: 679, gen1: 27, gen2: 3
// printfn $"Pi: %M{parallelPi 100UL 100000UL}" - Real: 00:00:03.360, CPU: 00:00:05.610, GC gen0: 680, gen1: 25, gen2: 4
// printfn $"Pi: %M{parallelPi 1000UL 10000UL}" - Real: 00:00:03.072, CPU: 00:00:05.400, GC gen0: 681, gen1: 35, gen2: 5
// printfn $"Pi: %M{parallelPi 10000UL 1000UL}" - Real: 00:00:03.095, CPU: 00:00:05.390, GC gen0: 733, gen1: 34, gen2: 5
// printfn $"Pi: %M{parallelPi 100000UL 100UL}" - Real: 00:00:03.309, CPU: 00:00:05.600, GC gen0: 745, gen1: 40, gen2: 3
// printfn $"Pi: %M{parallelPi 1000000UL 10UL}" - Real: 00:00:05.051, CPU: 00:00:08.560, GC gen0: 823, gen1: 79, gen2: 5

// In smaller workloads the change is negligible. Parallelism was actually slower in some cases.
// In larger workloads parallelism is alot faster. And it can be seen in the results that varying the amount of threads available greatly improves results
// Essentially paralellism is useful in large workloads where the overhead added by managing threads is neglible in the larger result. Which is in large workloads.