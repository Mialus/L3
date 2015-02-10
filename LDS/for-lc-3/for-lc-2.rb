#  ============================================================================
#  Definitions for the second lab class. Load 'animals.rb' before.

class Animal
  #
  def do_with_name
    puts "I'm #{@name}"
    yield(@name) if block_given?
  end
  #
  def gimme
    puts "I'm #{@name}, I want " + yield
  end
  #
end

$kingkong.do_with_name
$kingkong.do_with_name do |name| puts "#{name} and Jane" end
$kingkong.gimme do "Jane" end

$a = [22,1,2015]
$a.each do |x| p x end
$a.each_index do |index| p index end
p ($a.select do |x| x.odd? end)

p ([].inject(0) do |x,y| x + y end)
p ($a.inject(0) do |x,y| x + y end)
p ((1..10).inject(1) do |x,y| x * y end)

class Dog
  #
  def do_with_names_and(x)
    yield(@name,@ownername,x)
  end
  #
end
