def ph(s)

if s=~ /\A[A-Za-z]+(\(-?[0-9]\))?(\.[A-Za-z]+(\(-?[0-9])))?)*)Z\/
'$'+s.gsub(/:([A-Za-z]+)/,'\mathrm{\1}').gsub(((((-?[0-9])))/,^{\1}')+'$'

end
