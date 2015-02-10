$trace = false

class Fixnum
  def no_divisor_found?(a)
    catch (:exit) do
      a.all? do |presumed_divisor|
        if block_given? and yield(presumed_divisor)
          throw :exit,true
        else
          self % presumed_divisor != 0
        end
      end
    end
  end
end

$prime_number_thread = Thread::new do
  current = 1
  prime_number_a = []
  Thread::current[:current_prime_number] = current
  Thread::current[:prime_number_a] = prime_number_a
  sleep
  loop do
    current += 1
    the_floor_sqrt = Math::sqrt(current).floor 
    if the_floor_sqrt == 1 or
        current.no_divisor_found?(prime_number_a) do |presumed_divisor|
          presumed_divisor > the_floor_sqrt
        end
      puts "#{current} got..." if $trace
      prime_number_a.push(current)
      Thread::current[:current_prime_number] = current
      Thread::current[:prime_number_a] = prime_number_a
      sleep
    end
  end
end

def next_prime_number
  $prime_number_thread.wakeup unless $prime_number_thread.status == 'run'
  $prime_number_thread[:current_prime_number]
end

def next_prime_number_after(i)
  prime_number_got =\
    $prime_number_thread[:prime_number_a].find do |prime_number|
      prime_number > i
    end
  until prime_number_got
    next_prime_number_0 = next_prime_number
    prime_number_got = next_prime_number_0 > i ? next_prime_number_0 : nil
  end
  prime_number_got
end

    