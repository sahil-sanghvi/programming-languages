require "./prob1.rb"

# This script assumes that to_s is working correctly.

tests = []
f = MySet.new([1, 2, 3, 2, 1.5])
begin
  tests[0] = f.to_s == "[1, 1.5, 2, 3]"
rescue
  tests[0] = false
end
f.insert 3
f.insert -1
begin
  tests[1] = f.to_s == "[-1, 1, 1.5, 2, 3]"
rescue
  tests[1] = false
end

p = ""
f.each { |i| p += "_#{i}" }
begin
  tests[2] = p == '_-1_1_1.5_2_3'
rescue
  tests[2] = false
end
begin
  tests[3] = f.map { |x| x.round%2 } == [1, 1, 0, 0, 1]
rescue
  tests[3] = false
end
begin
  tests[4] = f.count == 5
rescue
  tests[4] = false
end
begin
  tests[5] = f.class == MySet
rescue
  tests[5] = false
end
begin
  tests[6] = f.is_a?(Array) == false
rescue
  tests[6] = false
end

begin
  tests[7] = (f | MySet.new([1, -2])).to_s == '[-2, -1, 1, 1.5, 2, 3]'
rescue
  tests[7] = false
end
begin
  tests[8] = (f & MySet.new([1, 2, 5])).to_s == '[1, 2]'
rescue
  tests[8] = false
end
begin
  tests[9] = f.to_s == '[-1, 1, 1.5, 2, 3]'
rescue
  tests[9] = false
end

f = MySet.new([1, 2, 3, 2, 1.5]) { |a,b| -(a<=>b) }
begin
  tests[10] = f.to_s == '[3, 2, 1.5, 1]'
rescue
  tests[10] = false
end

tests = tests.map {|i| if i then 1 else 0 end}
puts ['[csc330_tester]', 'initialize', tests[0]+tests[10]+tests[5]+tests[6], 4].join(" ")
puts ['[csc330_tester]', 'to_s', tests[0], 1].join(" ")
puts ['[csc330_tester]', 'insert', tests[1], 1].join(" ")
puts ['[csc330_tester]', 'each', tests[2]+tests[3]+tests[4], 3].join(" ")
puts ['[csc330_tester]', 'funcset', tests[7]+tests[8]+tests[9], 3].join(" ")
