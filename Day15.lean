import Lean
import Util
import Range

namespace Day15

open Lean Range

set_option maxRecDepth 1000000000

structure SensorBeacon : Type where
  sensor : Int × Int
  beacon : Int × Int 
deriving Repr

def rad : SensorBeacon → Nat := fun sb =>
  let dx := (sb.sensor.fst - sb.beacon.fst).natAbs
  let dy := (sb.sensor.snd - sb.beacon.snd).natAbs
  dx + dy

-- syntax int := num <|> "-"num

declare_syntax_cat int

syntax num : int
syntax "-"num : int

macro_rules
  | `(int| $x:num) => `(Int.ofNat $x)
  | `(int| -$x:num) => `(- (Int.ofNat $x))

instance : Coe (TSyntax `int) (TSyntax `term) where
  coe s := ⟨s.raw⟩


declare_syntax_cat sensor_beacon

syntax "Sensor at x=" int ", y=" int ": closest beacon is at x=" int ", y=" int : sensor_beacon

macro_rules
  | `(sensor_beacon| Sensor at x=$xS:int, y=$yS:int : closest beacon is at x=$xB:int, y=$yB:int) => 
      `({sensor := ($xS, $yS), beacon := ($xB, $yB)})

instance : Coe (TSyntax `sensor_beacon) (TSyntax `term) where
  coe s := ⟨s.raw⟩

syntax " (" sensor_beacon,* ") " : term

macro_rules
  | `(($sb:sensor_beacon)) => `([$sb])
  | `(($[$sb:sensor_beacon],*)) => `([$sb,*])

def sbs : List SensorBeacon := (
  Sensor at x=2, y=18: closest beacon is at x=-2, y=15,
  Sensor at x=9, y=16: closest beacon is at x=10, y=16,
  Sensor at x=13, y=2: closest beacon is at x=15, y=3,
  Sensor at x=12, y=14: closest beacon is at x=10, y=16,
  Sensor at x=10, y=20: closest beacon is at x=10, y=16,
  Sensor at x=14, y=17: closest beacon is at x=10, y=16,
  Sensor at x=8, y=7: closest beacon is at x=2, y=10,
  Sensor at x=2, y=0: closest beacon is at x=2, y=10,
  Sensor at x=0, y=11: closest beacon is at x=2, y=10,
  Sensor at x=20, y=14: closest beacon is at x=25, y=17,
  Sensor at x=17, y=20: closest beacon is at x=21, y=22,
  Sensor at x=16, y=7: closest beacon is at x=15, y=3,
  Sensor at x=14, y=3: closest beacon is at x=15, y=3,
  Sensor at x=20, y=1: closest beacon is at x=15, y=3
)

def getRange : SensorBeacon → Int → Range
  | ⟨s, b⟩, y₀ =>
    let r := rad ⟨s, b⟩
    let dy := Int.ofNat (s.snd - y₀).natAbs
    let dx := r - dy
    ⟨ s.fst - dx, s.fst + dx ⟩

#eval OpenSet.removeElement [getRange sbs[6] 10] 2
#eval OpenSet.removeElement [⟨ -2, 2 ⟩] 2

def solve : List SensorBeacon → Int → OpenSet := fun sbs y =>
  let rec helper : List SensorBeacon → OpenSet
    | [] => let x : OpenSet := []; x
    | sb :: sbs =>
      let r1 := getRange sb y
      if y == sb.beacon.snd then
        (OpenSet.removeElement [r1] sb.beacon.fst) ∪ helper sbs
      else
        [r1] ∪ helper sbs
  helper sbs

#eval OpenSet.size (solve sbs 10)

def real : List SensorBeacon := (
  Sensor at x=3428425, y=2345067: closest beacon is at x=3431988, y=2379841,
  Sensor at x=928237, y=25774: closest beacon is at x=1212315, y=-161555,
  Sensor at x=2061220, y=2396791: closest beacon is at x=2038311, y=2495160,
  Sensor at x=1830400, y=2994568: closest beacon is at x=1910058, y=3117415,
  Sensor at x=2485733, y=2625804: closest beacon is at x=2038311, y=2495160,
  Sensor at x=1855873, y=3971916: closest beacon is at x=1910058, y=3117415,
  Sensor at x=119582, y=3929652: closest beacon is at x=311197, y=4221202,
  Sensor at x=1069031, y=3509672: closest beacon is at x=1910058, y=3117415,
  Sensor at x=3368023, y=2213635: closest beacon is at x=3431988, y=2379841,
  Sensor at x=3713877, y=2460862: closest beacon is at x=3431988, y=2379841,
  Sensor at x=3593503, y=2174008: closest beacon is at x=3507689, y=2000000,
  Sensor at x=501760, y=93436: closest beacon is at x=1212315, y=-161555,
  Sensor at x=3712703, y=214999: closest beacon is at x=3507689, y=2000000,
  Sensor at x=1594824, y=2790273: closest beacon is at x=1910058, y=3117415,
  Sensor at x=2539549, y=3190814: closest beacon is at x=1910058, y=3117415,
  Sensor at x=3522790, y=2671548: closest beacon is at x=3431988, y=2379841,
  Sensor at x=1001452, y=1327490: closest beacon is at x=1212315, y=-161555,
  Sensor at x=629209, y=2451628: closest beacon is at x=-416149, y=2226089,
  Sensor at x=2636827, y=1146266: closest beacon is at x=3507689, y=2000000,
  Sensor at x=3909, y=625124: closest beacon is at x=1212315, y=-161555,
  Sensor at x=3950231, y=3688780: closest beacon is at x=3888160, y=3226725,
  Sensor at x=3449978, y=2328058: closest beacon is at x=3431988, y=2379841,
  Sensor at x=3974214, y=2582925: closest beacon is at x=3888160, y=3226725,
  Sensor at x=82663, y=3225533: closest beacon is at x=311197, y=4221202,
  Sensor at x=1958305, y=2292045: closest beacon is at x=2038311, y=2495160,
  Sensor at x=3465738, y=2123353: closest beacon is at x=3507689, y=2000000,
  Sensor at x=2940758, y=3884337: closest beacon is at x=2746166, y=4800483,
  Sensor at x=3429173, y=2275591: closest beacon is at x=3431988, y=2379841,
  Sensor at x=1527349, y=38565: closest beacon is at x=1212315, y=-161555,
  Sensor at x=3049925, y=2498038: closest beacon is at x=3431988, y=2379841,
  Sensor at x=1593202, y=3335178: closest beacon is at x=1910058, y=3117415,
  Sensor at x=3175520, y=3230234: closest beacon is at x=3888160, y=3226725
)

def f : Nat → Nat
  | 0 => 0
  | n+1 =>
    let curr := OpenSet.size (solve real n)
    f n + curr

-- #eval f 4000000

end Day15