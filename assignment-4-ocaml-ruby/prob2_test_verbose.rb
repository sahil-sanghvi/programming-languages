require "./prob2.rb"

passed = 0
failed = 0

def check(desc, result)
  if result
    puts "PASS: #{desc}"
    $passed += 1
  else
    puts "FAIL: #{desc}"
    $failed += 1
  end
end

$passed = 0
$failed = 0

ZERO = 0.0
ONE = 1.0
TWO = 2.0
THREE = 3.0
FOUR = 4.0
FIVE = 5.0
SIX = 6.0
SEVEN = 7.0
EIGHT = 8.0
NINE = 9.0
TEN = 10.0

# ==== equality helpers (copied from test file) ====
class GeometryExpression
  def real_close(r1, r2); (r1 - r2).abs < GeometryExpression::Epsilon; end
  def real_close_point(x1,y1,x2,y2); real_close(x1,x2) && real_close(y1,y2); end
end
class Nope < GeometryValue
  def ==(np); self.class == np.class; end
end
class Point < GeometryValue
  def ==(p); self.class==p.class && real_close_point(@x,@y,p.x,p.y); end
end
class Line < GeometryValue
  def ==(l); self.class==l.class && real_close_point(@m,@b,l.m,l.b); end
end
class VerticalLine < GeometryValue
  def ==(vl); self.class==vl.class && real_close(@x,vl.x); end
end
class LineSegment < GeometryValue
  def ==(ls)
    self.class==ls.class &&
    real_close_point(@x1,@y1,ls.x1,ls.y1) &&
    real_close_point(@x2,@y2,ls.x2,ls.y2)
  end
end
class Intersect < GeometryExpression
  attr_reader :e1, :e2
  def ==(i); self.class==i.class && @e1==i.e1 && @e2==i.e2; end
end
class Let < GeometryExpression
  attr_reader :s, :e1, :e2
  def ==(l); self.class==l.class && @s==l.s && @e1==l.e1 && @e2==l.e2; end
end
class Var < GeometryExpression
  attr_reader :s
  def ==(v); self.class==v.class && @s==v.s; end
end
class Shift < GeometryExpression
  attr_reader :dx, :dy, :e
  def ==(s); self.class==s.class && real_close_point(@dx,@dy,s.dx,s.dy) && @e==s.e; end
end

# ==== Point ====
puts "\n--- Point ---"
a = Point.new(THREE, FIVE)
check("Point initialized",             a.x == THREE && a.y == FIVE)
check("Point eval_prog returns self",  a.eval_prog([]) == a)
check("Point preprocess returns self", a.preprocess_prog == a)
a1 = a.shift(THREE, FIVE)
check("Point shift",                   a1.x == SIX && a1.y == TEN)
check("Point intersect same point",    a.intersect(Point.new(THREE, FIVE)) == Point.new(THREE, FIVE))
check("Point intersect diff point",    a.intersect(Point.new(FOUR, FIVE)).is_a?(Nope))

# ==== Line ====
puts "\n--- Line ---"
b = Line.new(THREE, FIVE)
check("Line initialized",             b.m == THREE && b.b == FIVE)
check("Line eval_prog returns self",  b.eval_prog([]) == b)
check("Line preprocess returns self", b.preprocess_prog == b)
b1 = b.shift(THREE, FIVE)
check("Line shift",                   b1.m == THREE && b1.b == ONE)
check("Line intersect same line",     b.intersect(Line.new(THREE, FIVE)).is_a?(Line))
check("Line intersect parallel",      b.intersect(Line.new(THREE, FOUR)).is_a?(Nope))

# ==== VerticalLine ====
puts "\n--- VerticalLine ---"
c = VerticalLine.new(THREE)
check("VerticalLine initialized",             c.x == THREE)
check("VerticalLine eval_prog returns self",  c.eval_prog([]) == c)
check("VerticalLine preprocess returns self", c.preprocess_prog == c)
c1 = c.shift(THREE, FIVE)
check("VerticalLine shift",                   c1.x == SIX)
check("VerticalLine intersect same",          c.intersect(VerticalLine.new(THREE)).is_a?(VerticalLine))
check("VerticalLine intersect diff",          c.intersect(VerticalLine.new(FOUR)).is_a?(Nope))

# ==== LineSegment ====
puts "\n--- LineSegment ---"
d = LineSegment.new(ONE, TWO, -THREE, -FOUR)
check("LineSegment eval_prog returns self", d.eval_prog([]) == d)
d1 = LineSegment.new(ONE, TWO, ONE, TWO)
d2 = d1.preprocess_prog
check("LineSegment same endpoints -> Point", d2.is_a?(Point) && d2.x == ONE && d2.y == TWO)
d = d.preprocess_prog
check("LineSegment preprocess reorders endpoints", d.x1 == ONE && d.y1 == TWO && d.x2 == -THREE && d.y2 == -FOUR)
d3 = d.shift(THREE, FIVE)
check("LineSegment shift", d3.x1 == FOUR && d3.y1 == SEVEN && d3.x2 == ZERO && d3.y2 == ONE)
d4 = d.intersect(LineSegment.new(ONE, TWO, -THREE, -FOUR))
check("LineSegment intersect same", d4.is_a?(LineSegment))
d5 = d.intersect(LineSegment.new(FOUR, FIVE, TWO, THREE).preprocess_prog)
check("LineSegment intersect disjoint", d5.is_a?(Nope))

# ==== Preprocess ====
puts "\n--- Preprocess ---"
check("test 0  Nope",          Nope.new.preprocess_prog == Nope.new)
check("test 1  Point",         Point.new(ONE,ONE).preprocess_prog == Point.new(ONE,ONE))
check("test 2  Line",          Line.new(ONE,TWO).preprocess_prog == Line.new(ONE,TWO))
check("test 3  VerticalLine",  VerticalLine.new(TWO).preprocess_prog == VerticalLine.new(TWO))
check("test 4  seg->point",    LineSegment.new(ONE,ONE,ONE,ONE).preprocess_prog == Point.new(ONE,ONE))
check("test 5  seg reorder v", LineSegment.new(ONE,ONE,ONE,TWO).preprocess_prog == LineSegment.new(ONE,TWO,ONE,ONE))
check("test 6  seg ok v",      LineSegment.new(ONE,ONE,ONE,ZERO).preprocess_prog == LineSegment.new(ONE,ONE,ONE,ZERO))
check("test 7  seg reorder h", LineSegment.new(ONE,ONE,TWO,ONE).preprocess_prog == LineSegment.new(TWO,ONE,ONE,ONE))
check("test 8  seg reorder",   LineSegment.new(ONE,ZERO,TWO,ONE).preprocess_prog == LineSegment.new(TWO,ONE,ONE,ZERO))
check("test 9  seg reorder",   LineSegment.new(ONE,ONE,TWO,ZERO).preprocess_prog == LineSegment.new(TWO,ZERO,ONE,ONE))
check("test 10 seg ok h",      LineSegment.new(TWO,ONE,ONE,ONE).preprocess_prog == LineSegment.new(TWO,ONE,ONE,ONE))
check("test 11 seg ok",        LineSegment.new(TWO,ZERO,ONE,ONE).preprocess_prog == LineSegment.new(TWO,ZERO,ONE,ONE))
check("test 12 seg ok",        LineSegment.new(TWO,ONE,ONE,ZERO).preprocess_prog == LineSegment.new(TWO,ONE,ONE,ZERO))
check("test 13 float close",   LineSegment.new(1.00000999,ONE,ONE,TWO).preprocess_prog == LineSegment.new(ONE,TWO,1.00000999,ONE))
check("test 14 Let preprocess", Let.new("x",LineSegment.new(ONE,ONE,ONE,ONE),Var.new("x")).preprocess_prog == Let.new("x",Point.new(ONE,ONE),Var.new("x")))
check("test 15 Shift preprocess", Shift.new(ONE,ONE,LineSegment.new(ONE,ONE,ONE,ONE)).preprocess_prog == Shift.new(ONE,ONE,Point.new(ONE,ONE)))
check("test 16 Intersect preprocess", Intersect.new(LineSegment.new(ONE,ONE,ONE,ONE),LineSegment.new(TWO,ONE,ONE,ZERO)).preprocess_prog == Intersect.new(Point.new(ONE,ONE),LineSegment.new(TWO,ONE,ONE,ZERO)))
check("test 80 Let nested",    Let.new("a",LineSegment.new(3.2,4.1,3.2,4.1),LineSegment.new(3.2,4.1,3.2,4.1)).preprocess_prog == Let.new("a",Point.new(3.2,4.1),Point.new(3.2,4.1)))
check("test 81 Shift+seg",     Shift.new(ONE,TWO,LineSegment.new(3.2,4.1,3.2,4.1)).preprocess_prog == Shift.new(ONE,TWO,Point.new(3.2,4.1)))
check("test 82 Intersect seg", Intersect.new(LineSegment.new(3.2,4.1,3.2,4.1),LineSegment.new(3.2,4.1,3.2,4.1)).preprocess_prog == Intersect.new(Point.new(3.2,4.1),Point.new(3.2,4.1)))

# ==== Shift eval ====
puts "\n--- Shift eval ---"
check("shift Point",        Shift.new(ONE,ONE,Point.new(TWO,THREE)).eval_prog([]) == Point.new(THREE,FOUR))
check("shift Line",         Shift.new(TWO,10.3,Line.new(THREE,FIVE)).eval_prog([]) == Line.new(THREE,9.3))
check("shift VerticalLine", Shift.new(ONE,TWO,VerticalLine.new(FIVE)).eval_prog([]) == VerticalLine.new(6.0))
check("shift seg->point",   Shift.new(TWO,TWO,LineSegment.new(ONE,ONE,ONE,ONE)).preprocess_prog.eval_prog([]) == Point.new(THREE,THREE))
check("shift LineSegment",  Shift.new(ONE,TWO,LineSegment.new(ONE,ONE,THREE,THREE)).preprocess_prog.eval_prog([]) == LineSegment.new(FOUR,FIVE,TWO,THREE))
check("shift nested",       Shift.new(ONE,ONE,Shift.new(ONE,ONE,Shift.new(ONE,ONE,Point.new(ONE,ONE)))).preprocess_prog.eval_prog([]) == Point.new(FOUR,FOUR))
check("shift Point 83",     Shift.new(THREE,FOUR,Point.new(FOUR,FOUR)).eval_prog([]) == Point.new(SEVEN,EIGHT))
check("shift Line 84",      Shift.new(THREE,FOUR,Line.new(FOUR,FOUR)).eval_prog([]) == Line.new(FOUR,-FOUR))
check("shift VLine 85",     Shift.new(THREE,FOUR,VerticalLine.new(FOUR)).eval_prog([]) == VerticalLine.new(SEVEN))
check("shift Seg 86",       Shift.new(THREE,FOUR,LineSegment.new(FOUR,THREE,12.0,-TWO)).eval_prog([]) == LineSegment.new(SEVEN,SEVEN,15.0,TWO))

# ==== Let eval ====
puts "\n--- Let eval ---"
check("Let Point",      Let.new("x",Point.new(TWO,THREE),Var.new("x")).preprocess_prog.eval_prog([]) == Point.new(TWO,THREE))
check("Let Line",       Let.new("x",Line.new(THREE,FIVE),Var.new("x")).preprocess_prog.eval_prog([]) == Line.new(THREE,FIVE))
check("Let VLine",      Let.new("x",VerticalLine.new(ONE),Var.new("x")).preprocess_prog.eval_prog([]) == VerticalLine.new(ONE))
check("Let Seg",        Let.new("x",LineSegment.new(TWO,TWO,ONE,ONE),Var.new("x")).preprocess_prog.eval_prog([]) == LineSegment.new(TWO,TWO,ONE,ONE))
check("Let shadow",     Let.new("x",Point.new(ONE,ONE),Let.new("x",Point.new(TWO,TWO),Var.new("x"))).preprocess_prog.eval_prog([]) == Point.new(TWO,TWO))
check("Let+Shift 40",   Let.new("x",LineSegment.new(ONE,ONE,ONE,ONE),Shift.new(ONE,ONE,Var.new("x"))).preprocess_prog.eval_prog([]) == Point.new(TWO,TWO))
check("Shift+Let 41",   Shift.new(ONE,ONE,Let.new("x",VerticalLine.new(TWO),Var.new("x"))).preprocess_prog.eval_prog([]) == VerticalLine.new(THREE))

# ==== Intersect: Nope ====
puts "\n--- Intersect: Nope ---"
check("Nope∩Nope",    Intersect.new(Nope.new,Nope.new).preprocess_prog.eval_prog([]) == Nope.new)
check("Nope∩Point",   Intersect.new(Nope.new,Point.new(ONE,TWO)).preprocess_prog.eval_prog([]) == Nope.new)
check("Nope∩Line",    Intersect.new(Nope.new,Line.new(ONE,ONE)).preprocess_prog.eval_prog([]) == Nope.new)
check("Nope∩VLine",   Intersect.new(Nope.new,VerticalLine.new(ONE)).preprocess_prog.eval_prog([]) == Nope.new)
check("Nope∩Seg",     Intersect.new(Nope.new,LineSegment.new(ONE,ONE,THREE,THREE)).preprocess_prog.eval_prog([]) == Nope.new)

# ==== Intersect: Point ====
puts "\n--- Intersect: Point ---"
check("Pt∩Pt close",   Intersect.new(Point.new(ONE,ONE),Point.new(1.00000999,ONE)).preprocess_prog.eval_prog([]) == Point.new(1.00000999,ONE))
check("Pt∩Pt far",     Intersect.new(Point.new(ONE,ONE),Point.new(1.0999,ONE)).preprocess_prog.eval_prog([]) == Nope.new)
check("Pt on Line",    Intersect.new(Point.new(ONE,FOUR),Line.new(ONE,THREE)).preprocess_prog.eval_prog([]) == Point.new(ONE,FOUR))
check("Pt off Line",   Intersect.new(Point.new(TWO,FOUR),Line.new(ONE,THREE)).preprocess_prog.eval_prog([]) == Nope.new)
check("Pt on VLine",   Intersect.new(Point.new(TWO,FOUR),VerticalLine.new(TWO)).preprocess_prog.eval_prog([]) == Point.new(TWO,FOUR))
check("Pt off VLine",  Intersect.new(Point.new(FOUR,FOUR),VerticalLine.new(TWO)).preprocess_prog.eval_prog([]) == Nope.new)
check("Pt in Seg",     Intersect.new(Point.new(TWO,TWO),LineSegment.new(ZERO,ZERO,THREE,THREE)).preprocess_prog.eval_prog([]) == Point.new(TWO,TWO))
check("Pt out Seg",    Intersect.new(Point.new(FOUR,FOUR),LineSegment.new(ZERO,ZERO,THREE,THREE)).preprocess_prog.eval_prog([]) == Nope.new)

# ==== Intersect: Line ====
puts "\n--- Intersect: Line ---"
check("Line∩Line point",   Intersect.new(Line.new(ONE,TWO),Line.new(TWO,ZERO)).preprocess_prog.eval_prog([]) == Point.new(TWO,FOUR))
check("Line∩Line parallel", Intersect.new(Line.new(ONE,TWO),Line.new(ONE,THREE)).preprocess_prog.eval_prog([]) == Nope.new)
check("Line∩VLine",         Intersect.new(Line.new(ONE,TWO),VerticalLine.new(TWO)).preprocess_prog.eval_prog([]) == Point.new(TWO,FOUR))
check("Line∩Seg on line",   Intersect.new(Line.new(ONE,ZERO),LineSegment.new(ZERO,ZERO,FOUR,FOUR)).preprocess_prog.eval_prog([]) == LineSegment.new(FOUR,FOUR,ZERO,ZERO))
check("Line∩Seg one pt",    Intersect.new(Line.new(TWO,ZERO),LineSegment.new(ZERO,ZERO,FOUR,FOUR)).preprocess_prog.eval_prog([]) == Point.new(ZERO,ZERO))
check("Line∩Seg none",      Intersect.new(Line.new(TWO,ZERO),LineSegment.new(ONE,ONE,FOUR,FOUR)).preprocess_prog.eval_prog([]) == Nope.new)

# ==== Intersect: VerticalLine ====
puts "\n--- Intersect: VerticalLine ---"
check("VLine∩VLine diff",  Intersect.new(VerticalLine.new(ONE),VerticalLine.new(TWO)).preprocess_prog.eval_prog([]) == Nope.new)
check("VLine∩Seg on line", Intersect.new(VerticalLine.new(ONE),LineSegment.new(ONE,ZERO,ONE,FOUR)).preprocess_prog.eval_prog([]) == LineSegment.new(ONE,FOUR,ONE,ZERO))
check("VLine∩Seg one pt",  Intersect.new(VerticalLine.new(ONE),LineSegment.new(ZERO,ONE,THREE,ONE)).preprocess_prog.eval_prog([]) == Point.new(ONE,ONE))
check("VLine∩Seg none",    Intersect.new(VerticalLine.new(ONE),LineSegment.new(TWO,TWO,FOUR,FOUR)).preprocess_prog.eval_prog([]) == Nope.new)

# ==== Intersect: LineSegment ====
puts "\n--- Intersect: LineSegment (oblique) ---"
check("Seg∩Seg parallel v",   Intersect.new(LineSegment.new(ZERO,ZERO,ZERO,FOUR),LineSegment.new(ONE,ZERO,ONE,FOUR)).preprocess_prog.eval_prog([]) == Nope.new)
check("Seg∩Seg parallel h",   Intersect.new(LineSegment.new(ZERO,ZERO,FOUR,ZERO),LineSegment.new(ZERO,ONE,FOUR,ONE)).preprocess_prog.eval_prog([]) == Nope.new)
check("Seg∩Seg cross",        Intersect.new(LineSegment.new(TWO,ZERO,TWO,FOUR),LineSegment.new(ZERO,TWO,FOUR,TWO)).preprocess_prog.eval_prog([]) == Point.new(TWO,TWO))
check("Seg∩Seg b in a (v)",   Intersect.new(LineSegment.new(ZERO,ZERO,ZERO,FOUR),LineSegment.new(ZERO,ONE,ZERO,THREE)).preprocess_prog.eval_prog([]) == LineSegment.new(ZERO,THREE,ZERO,ONE))
check("Seg∩Seg a in b (v)",   Intersect.new(LineSegment.new(ZERO,ONE,ZERO,THREE),LineSegment.new(ZERO,ZERO,ZERO,FOUR)).preprocess_prog.eval_prog([]) == LineSegment.new(ZERO,THREE,ZERO,ONE))
check("Seg∩Seg same (v)",     Intersect.new(LineSegment.new(ZERO,ONE,ZERO,THREE),LineSegment.new(ZERO,ONE,ZERO,THREE)).preprocess_prog.eval_prog([]) == LineSegment.new(ZERO,THREE,ZERO,ONE))
check("Seg∩Seg overlap (v)",  Intersect.new(LineSegment.new(ZERO,ZERO,ZERO,THREE),LineSegment.new(ZERO,ONE,ZERO,FOUR)).preprocess_prog.eval_prog([]) == LineSegment.new(ZERO,THREE,ZERO,ONE))
check("Seg∩Seg disjoint obl", Intersect.new(LineSegment.new(ONE,ONE,FOUR,FOUR),LineSegment.new(4.1,4.1,FIVE,FIVE)).preprocess_prog.eval_prog([]).class == Nope.new.class)
check("Seg∩Seg b in a obl",   Intersect.new(LineSegment.new(ONE,ONE,FOUR,FOUR),LineSegment.new(TWO,TWO,THREE,THREE)).preprocess_prog.eval_prog([]) == LineSegment.new(THREE,THREE,TWO,TWO))
check("Seg∩Seg overlap obl",  Intersect.new(LineSegment.new(ONE,ONE,FOUR,FOUR),LineSegment.new(-ONE,-ONE,THREE,THREE)).preprocess_prog.eval_prog([]) == LineSegment.new(THREE,THREE,ONE,ONE))
check("Seg∩Seg overlap obl2", Intersect.new(LineSegment.new(ONE,ONE,FOUR,FOUR),LineSegment.new(TWO,TWO,FIVE,FIVE)).preprocess_prog.eval_prog([]) == LineSegment.new(FOUR,FOUR,TWO,TWO))
check("Seg∩Seg touch right",  Intersect.new(LineSegment.new(ONE,ONE,FOUR,FOUR),LineSegment.new(FOUR,FOUR,FIVE,FIVE)).preprocess_prog.eval_prog([]) == Point.new(FOUR,FOUR))
check("Seg∩Seg touch left",   Intersect.new(LineSegment.new(ONE,ONE,FOUR,FOUR),LineSegment.new(-FOUR,-FOUR,ONE,ONE)).preprocess_prog.eval_prog([]) == Point.new(ONE,ONE))
check("Seg∩Seg a in b obl",   Intersect.new(LineSegment.new(TWO,TWO,THREE,THREE),LineSegment.new(ONE,ONE,FIVE,FIVE)).preprocess_prog.eval_prog([]) == LineSegment.new(THREE,THREE,TWO,TWO))

puts "\n--- Intersect: two vertical LineSegments ---"
check("vSeg∩vSeg disjoint",   Intersect.new(LineSegment.new(ONE,ONE,ONE,FOUR),LineSegment.new(ONE,4.1,ONE,FIVE)).preprocess_prog.eval_prog([]).class == Nope.new.class)
check("vSeg∩vSeg b in a",     Intersect.new(LineSegment.new(ONE,ONE,ONE,FOUR),LineSegment.new(ONE,TWO,ONE,THREE)).preprocess_prog.eval_prog([]) == LineSegment.new(ONE,THREE,ONE,TWO))
check("vSeg∩vSeg overlap",    Intersect.new(LineSegment.new(ONE,ONE,ONE,FOUR),LineSegment.new(ONE,-ONE,ONE,THREE)).preprocess_prog.eval_prog([]) == LineSegment.new(ONE,THREE,ONE,ONE))
check("vSeg∩vSeg overlap2",   Intersect.new(LineSegment.new(ONE,ONE,ONE,FOUR),LineSegment.new(ONE,TWO,ONE,FIVE)).preprocess_prog.eval_prog([]) == LineSegment.new(ONE,FOUR,ONE,TWO))
check("vSeg∩vSeg touch top",  Intersect.new(LineSegment.new(ONE,ONE,ONE,FOUR),LineSegment.new(ONE,FOUR,ONE,FIVE)).preprocess_prog.eval_prog([]) == Point.new(ONE,FOUR))
check("vSeg∩vSeg touch bot",  Intersect.new(LineSegment.new(ONE,ONE,ONE,FOUR),LineSegment.new(ONE,-FOUR,ONE,ONE)).preprocess_prog.eval_prog([]) == Point.new(ONE,ONE))
check("vSeg∩vSeg a in b",     Intersect.new(LineSegment.new(ONE,TWO,ONE,THREE),LineSegment.new(ONE,ONE,ONE,FIVE)).preprocess_prog.eval_prog([]) == LineSegment.new(ONE,THREE,ONE,TWO))

puts "\n==============================="
puts "PASSED: #{$passed}"
puts "FAILED: #{$failed}"
puts "TOTAL:  #{$passed + $failed}"
puts "==============================="
