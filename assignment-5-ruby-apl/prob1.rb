# V01039545 - Sahil Sanghvi
# CSC 330 - Assignment 5, Problem 1: Fumix (MySet)

module FuncSet
  def |(other) # union
    self.class.new(entries + other.entries) { |x, y| cmp(x, y) }
  end
  def &(other) # intersect
    common = self.map { |x| other.find { |y| self.cmp(x, y) == 0 } }
    self.class.new(common) { |x, y| cmp(x, y) }
  end
end

# MySet: a polymorphic functional set backed by a sorted array.
# Elements are unique and kept in the order defined by the comparator.
class MySet
  include Enumerable
  include FuncSet

  # Takes an optional array of initial items and an optional block as the comparator.
  # If no block is given, falls back to the spaceship operator <=> for default ordering.
  def initialize(items = [], &block)
    @cmp = block_given? ? block : ->(x, y) { x <=> y }
    @entries = []

    # Use insert for each item so uniqueness + ordering are enforced from the start
    items.each { |item| insert(item) }
  end

  # Delegates comparison to whatever comparator was provided at construction
  def cmp(x, y)
    @cmp.call(x, y)
  end

  # Exposes the internal array so the FuncSet mixin can build new sets from it
  def entries
    @entries
  end

  # Returns the set as a string in array format, e.g. "[1, 2, 3]"
  # We call Array#to_s explicitly because puts behaves oddly with raw arrays
  def to_s
    @entries.to_s
  end

  # Inserts an element into the correct sorted position.
  # Skips nil and any element already present (determined by comparator returning 0).
  def insert(item)
    return if item.nil?

    # Duplicate check: if cmp returns 0 for any existing element, skip insertion
    return if @entries.any? { |e| cmp(e, item) == 0 }

    # Linear scan to find the first element that is "greater than" the new item.
    # Inserting before that position keeps the array sorted by our comparator.
    index = @entries.index { |e| cmp(e, item) > 0 }

    if index.nil?
      # New item is the largest so far; append to the end
      @entries << item
    else
      @entries.insert(index, item)
    end
  end

  # Required by Enumerable — just delegates to the internal array's each
  def each(&block)
    @entries.each(&block)
  end
end

# Bonus (Q6): Is map safe to use?
#
# No, not entirely. Enumerable#map returns a plain Array, not a MySet.