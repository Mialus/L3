#Wargnier Pierre

class Array #ajout de stammer à la classe Array
 def stammer
   taille = self.length
   i2 = 0
   tab=[]
    for i in 0..(taille-1) do 
      for j in 0..i2 do 
           tab.push(self.at(i))
      end
      i2+=1
     end
    tab
end
end

p [20,2,2015].stammer

def increasing?(a) #creation de increasing
  test = true
  for i in 0..a.length-2
    test = false if a[i] >= a[i+1]
  end
  test
end

a=[20,2,2015]
p increasing?([20,2,2015,2016])
p increasing?([2,20,2015,2016])

def monotone?(a) #creation de monotone
yield(@operation) if block_given?
test = true
	for i in 0..a.length-2
		test = true if  @operation
	end
	test
end

p monotone?([20,2,2015,2016]) do |x,y| x < y end
p monotone?([2,20,2015,2016]) do |x,y| x < y end
p monotone?([2015,2015,2015,2015]) do |x,y| x == y end


def increasing_v2?(a) #creation increasing
end
def decreasing?(a) #creation de decreasing
end

#p increasing_v2?([20,2,2015,2016])
#p increasing_v2?([2,20,2015,2016])

def substitute_with_dot(s) # Expression reguliére Numéro de téléphone
    puts s.sub(/(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})/, '\1.\2.\3.\4.\5')
end
    
substitute_with_dot('0479621501')

def substitute_with_international_prefix(s)
    puts s.sub(/(\d)(\d)(\d{2})(\d{2})(\d{2})(\d{2})/, '(+33)[\1]\2.\3.\4.\5.\6')     
end
substitute_with_international_prefix('0479621501')

