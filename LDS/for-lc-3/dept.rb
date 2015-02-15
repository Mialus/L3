
def dept(my_match)

if(my_match =~ /\A(0[1-9]|[13-8][0-9]|2[AB1-9]|9([0-5]|7[1-96]))\Z/) == 0
	my_match
else
	"ce n est pas un departement !"
end
end
