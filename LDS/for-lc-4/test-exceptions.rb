def testexceptions(s)
  begin
    begin
      eval s
    rescue ZeroDivisionError
      p 0
      1
    else
      p 2
      3
    ensure
      p 4
      5
    end
  rescue RuntimeError,StandardError => e
    p e.message
    p 6
    7
  else
    p 8
    9
  ensure
    p 10
    11
  end
end

p(testexceptions("2014"))
p(testexceptions("1/0"))
p(testexceptions "open('nonexistingfile')")

def testexceptionsv2
  begin
    begin
      yield
    rescue ZeroDivisionError
      p 0
      1
    else
      p 2
      3
    ensure
      p 4
      5
    end
  rescue RuntimeError,StandardError => e
    p e.message
    p 6
    7
  else
    p 8
    9
  ensure
    p 10
    11
  end
end

p(testexceptionsv2 do p "OK"
  2014
end)

p(testexceptionsv2 do p "Division by zero"
  1 / 0
end)

p(testexceptionsv2 do p "Opening file failed"
  open 'nonexistingfile'
end)
