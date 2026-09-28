# Assignment 4
# Do not make changes to this code except where you see comments containing the word TODO.
# V-number: V01039545
# Name: Sahil Sanghvi

class GeometryExpression
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue
  # Do *not* change methods in this class definition; you can add methods if you wish

  private

  def real_close(r1, r2)
    (r1 - r2).abs < GeometryExpression::Epsilon
  end

  def real_close_point(x1, y1, x2, y2)
    real_close(x1, x2) && real_close(y1, y2)
  end

  def two_points_to_line(x1, y1, x2, y2)
    if real_close(x1, x2)
      VerticalLine.new x1
    else
      m = (y1 - y2).to_f / (x1 - x2)
      b = y2 - m * x2
      Line.new(m, b)
    end
  end

  public

  def intersectNope(np)
    np
  end

  def intersectLineSegment(seg)
    line_result = intersect(two_points_to_line(seg.x1, seg.y1, seg.x2, seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class Nope < GeometryValue
  def eval_prog(env)
    self
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    self
  end

  def intersect(other)
    other.intersectNope self
  end

  def intersectPoint(p)
    self
  end

  def intersectLine(line)
    self
  end

  def intersectVerticalLine(vline)
    self
  end

  def intersectWithSegmentAsLineResult(seg)
    self
  end
end

class Point < GeometryValue
  attr_reader :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def eval_prog(env)
    self
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    Point.new(@x + dx, @y + dy)
  end

  def intersect(other)
    other.intersectPoint self
  end

  def intersectPoint(p)
    # self is another Point; intersection is self if same point, else Nope
    if real_close_point(@x, @y, p.x, p.y)
      self
    else
      Nope.new
    end
  end

  def intersectLine(line)
    # intersection of line with self (a Point): check if point is on line
    line.intersectPoint self
  end

  def intersectVerticalLine(vl)
    # intersection of vertical line with self (a Point): check x matches
    vl.intersectPoint self
  end

  def intersectWithSegmentAsLineResult(seg)
    # self is a Point that is on the line containing seg.
    # Return self if it is within the segment bounds, else Nope.
    eps = GeometryExpression::Epsilon
    inbetween = lambda do |v, end1, end2|
      ((end1 - eps) <= v && v <= (end2 + eps)) ||
      ((end2 - eps) <= v && v <= (end1 + eps))
    end
    if inbetween.call(@x, seg.x1, seg.x2) && inbetween.call(@y, seg.y1, seg.y2)
      self
    else
      Nope.new
    end
  end
end

class Line < GeometryValue
  attr_reader :m, :b

  def initialize(m, b)
    @m = m
    @b = b
  end

  def eval_prog(env)
    self
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    Line.new(@m, @b + dy - @m * dx)
  end

  def intersect(other)
    other.intersectLine self
  end

  def intersectPoint(p)
    # Check if point p is on self (a Line)
    if real_close(p.y, @m * p.x + @b)
      p
    else
      Nope.new
    end
  end

  def intersectLine(line)
    # line is the "other" Line; self is being intersected with it
    # double dispatch: line.intersectLine(self) means we intersect line (the arg) with self
    if real_close(@m, line.m)
      if real_close(@b, line.b)
        self  # same line
      else
        Nope.new  # parallel
      end
    else
      x = (line.b - @b).to_f / (@m - line.m)
      y = @m * x + @b
      Point.new(x, y)
    end
  end

  def intersectVerticalLine(vl)
    # vl is a VerticalLine; self is a Line
    vl.intersectLine self
  end

  def intersectWithSegmentAsLineResult(seg)
    # self (a Line) is the intersection of some shape and the line containing seg.
    # Return the intersection of that shape and seg, which equals self intersect seg = seg
    # (since self is on the line containing seg, the whole segment is the result)
    seg
  end
end

class VerticalLine < GeometryValue
  attr_reader :x

  def initialize(x)
    @x = x
  end

  def eval_prog(env)
    self
  end

  def preprocess_prog
    self
  end

  def shift(dx, dy)
    VerticalLine.new(@x + dx)
  end

  def intersect(other)
    other.intersectVerticalLine self
  end

  def intersectPoint(p)
    if real_close(p.x, @x)
      p
    else
      Nope.new
    end
  end

  def intersectLine(line)
    # intersection of a non-vertical line with self (vertical line x=@x)
    Point.new(@x, line.m * @x + line.b)
  end

  def intersectVerticalLine(vl)
    # vl is the other VerticalLine; self is self
    if real_close(@x, vl.x)
      self
    else
      Nope.new
    end
  end

  def intersectWithSegmentAsLineResult(seg)
    # self (a VerticalLine) contains seg; return seg
    seg
  end
end

class LineSegment < GeometryValue
  attr_reader :x1, :y1, :x2, :y2

  def initialize(x1, y1, x2, y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def eval_prog(env)
    self
  end

  def preprocess_prog
    # If endpoints are float-close, reduce to a Point
    if real_close_point(@x1, @y1, @x2, @y2)
      return Point.new(@x1, @y1)
    end
    # Reorder: higher-x first; if same x, higher-y first
    if real_close(@x1, @x2)
      # vertical segment: higher y first
      if @y1 > @y2
        LineSegment.new(@x1, @y1, @x2, @y2)
      else
        LineSegment.new(@x2, @y2, @x1, @y1)
      end
    else
      # non-vertical: higher x first
      if @x1 > @x2
        LineSegment.new(@x1, @y1, @x2, @y2)
      else
        LineSegment.new(@x2, @y2, @x1, @y1)
      end
    end
  end

  def shift(dx, dy)
    LineSegment.new(@x1 + dx, @y1 + dy, @x2 + dx, @y2 + dy)
  end

  def intersect(other)
    other.intersectLineSegment self
  end

  def intersectPoint(p)
    p.intersectLineSegment self
  end

  def intersectLine(line)
    line.intersectLineSegment self
  end

  def intersectVerticalLine(vl)
    vl.intersectLineSegment self
  end

  def intersectWithSegmentAsLineResult(seg)
    # self is the result of intersecting some shape with the line containing seg.
    # self is also a LineSegment on that same line (since the shape was a LineSegment too).
    # We compute the overlap of self and seg, both on the same line/vertical-line.
    eps = GeometryExpression::Epsilon
    if real_close(@x1, @x2)
      # Vertical segments: compare by y
      # Let a start lower (smaller y2 = start), b start higher
      if @y2 < seg.y2  # self starts lower
        a_x_end, a_y_end, a_x_start, a_y_start = @x1, @y1, @x2, @y2
        b_x_end, b_y_end, b_x_start, b_y_start = seg.x1, seg.y1, seg.x2, seg.y2
      else
        a_x_end, a_y_end, a_x_start, a_y_start = seg.x1, seg.y1, seg.x2, seg.y2
        b_x_end, b_y_end, b_x_start, b_y_start = @x1, @y1, @x2, @y2
      end
      if real_close(a_y_end, b_y_start)
        Point.new(a_x_end, a_y_end)  # just touching
      elsif a_y_end < b_y_start
        Nope.new  # disjoint
      elsif a_y_end > b_y_end
        LineSegment.new(b_x_end, b_y_end, b_x_start, b_y_start)  # b inside a
      else
        LineSegment.new(a_x_end, a_y_end, b_x_start, b_y_start)  # overlapping
      end
    else
      # Non-vertical segments: compare by x
      # Let a start to the left (smaller x2 = start), b start to the right
      if @x2 < seg.x2  # self starts to the left
        a_x_end, a_y_end, a_x_start, a_y_start = @x1, @y1, @x2, @y2
        b_x_end, b_y_end, b_x_start, b_y_start = seg.x1, seg.y1, seg.x2, seg.y2
      else
        a_x_end, a_y_end, a_x_start, a_y_start = seg.x1, seg.y1, seg.x2, seg.y2
        b_x_end, b_y_end, b_x_start, b_y_start = @x1, @y1, @x2, @y2
      end
      if real_close(a_x_end, b_x_start)
        Point.new(a_x_end, a_y_end)  # just touching
      elsif a_x_end < b_x_start
        Nope.new  # disjoint
      elsif a_x_end > b_x_end
        LineSegment.new(b_x_end, b_y_end, b_x_start, b_y_start)  # b inside a
      else
        LineSegment.new(a_x_end, a_y_end, b_x_start, b_y_start)  # overlapping
      end
    end
  end
end

class Intersect < GeometryExpression
  def initialize(e1, e2)
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end

  def eval_prog(env)
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end
end

class Let < GeometryExpression
  def initialize(s, e1, e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end

  def eval_prog(env)
    # Evaluate e1, bind to s, then evaluate e2 in extended env (no mutation)
    v1 = @e1.eval_prog(env)
    @e2.eval_prog([[@s, v1]] + env)
  end
end

class Var < GeometryExpression
  def initialize(s)
    @s = s
  end

  def eval_prog(env) # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end

  def preprocess_prog
    self
  end
end

class Shift < GeometryExpression
  def initialize(dx, dy, e)
    @dx = dx
    @dy = dy
    @e = e
  end

  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end

  def eval_prog(env)
    @e.eval_prog(env).shift(@dx, @dy)
  end
end
