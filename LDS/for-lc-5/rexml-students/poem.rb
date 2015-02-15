class Poem
  attr_reader\
    :dtd_s,:title,:collection,:poem_date,:author_lastname,:author_firstname,
    :author_birthdate,:author_deathdate,:body
  def initialize(metadata_0,title_0,body_0)
    [:dtd_s,:collection,:poem_date,:author_lastname,:author_firstname,
     :author_birthdate,:author_deathdate].each do |symbol|
      symbol_s = symbol.to_s
      eval("#{'@' + symbol_s} = metadata_0[#{':' + symbol_s}]")
    end
    @title = title_0
    @body = body_0
  end
end
