require 'rexml/parsers/sax2parser'
require 'rexml/sax2listener'
include REXML

class Get_Infos_Parser
  #
  include REXML::SAX2Listener
  #
  def initialize
    #  ...
  end
  #
  #  ...
  #
  def self.indent_nb
    #  ...
  end
  #
  def self.char_nb_minmax
    #  ...
  end
  #
  def start_element(uri,localname,qname,attributes)
    #  ...
  end
  #
  def end_element(uri,localname,qname)
    #  ...
  end
  #
  def characters(s)
    #  ...
  end
  #
end

def get_infos(input_filename_a)
  input_filename_a.each do |input_filename|
    begin
      parser = Parsers::SAX2Parser::new(File::new(input_filename))
      parser.listen(Get_Infos_Parser::new)
      parser.parse
    rescue StandardError => e
      puts e.message
    end
  end
  #  ...
end

get_infos(ARGV)
