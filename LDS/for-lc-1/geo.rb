class Quadrilateral
  def initialize(ml0,ml1,ml2,ml3,ma0,ma1,ma2)
    @l0 = ml0   #  "@...":  instance variable
    @l1 = ml1   #  "@...":  instance variable
    @l2 = ml2   #  "@...":  instance variable
    @l3 = ml3   #  "@...":  instance variable
    @a0 = ma0   #  "@...":  instance variable
    @a1 = ma1   #  "@...":  instance variable
    @a2 = ma2   #  "@...":  instance variable
  end
  def peri
    p= l0+l1+l2+l3
  end
end

class Parallelogram < Quadrilateral
  def initialize(ml0,ml1,ma0)
    super(ml0,ml1,ml0,ml1,ma0,(MATH::PI-ma0),ma0)
  end
  def peri
    p=2*(l0+l1)
  end
  def sur
    s=l0*l1*MATH::COS(a0-(MATH::PI/2))
  end
end

class Rectangle < Parallelogram
  def initialize(ml0,ml1)
    super(ml0,ml1,(MATH::PI/2))
  end
  def sur
    s=l0*l1
  end
end

class Carre < Rectangle
  def initialize(ml0,ml1)
    super(ml0,ml1,(MATH::PI/2))
  end
  def peri
    p=4*l0
  end
end
