class Quadrilateral
  attr_reader\
    :length_0, :length_1, :length_2, :length_3, :angle_0, :angle_1, :angle_2
  def initialize(l0,l1,l2,l3,a0,a1,a2)
    @length_0 = l0
    @length_1 = l1
    @length_2 = l2
    @length_3 = l3
    @angle_0 = a0
    @angle_1 = a1
    @angle_2 = a2
  end
  def perimeter
    @length_0 + @length_1 + @length_2 + @length_3
  end
  def surface
    nil
  end
end

class Parallelogram < Quadrilateral
  undef :length_2, :length_3, :angle_1, :angle_2
  def initialize(l0,l1,a0)
    super(l0,l1,l0,l1,a0,Math::PI - a0,a0)
  end
  def perimeter
    2.0 * (@length_0 + @length_1)
  end
  def surface
    @length_0 * @length_1 * Math::cos(@angle_0 - Math::PI / 2.0)
  end
end

class Rectangle < Parallelogram
  undef :angle_0
  def initialize(l0,l1)
    super(l0,l1,Math::PI / 2.0)
  end
  def surface
    @length_0 * @length_1
  end
end

class Square < Rectangle
  undef :length_1
  def initialize(l0)
    super(l0,l0)
  end
  def perimeter
    4.0 * @length_0
  end
end

class Lozange < Parallelogram
  undef :length_1
  def initialize(l0)
    super(l0,l0)
  end
  def perimeter
    4.0 * @length_0
  end
end
