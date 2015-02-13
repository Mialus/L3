require 'rexml/document'
include REXML

module Ruby2xml
  #
  def Ruby2xml.make_preamble(poem_0)
    preamble_e = Element::new('preambule')
    preamble_e.add(Ruby2xml.make_leaf(poem_0.title,'titre'))
    preamble_e.add(Ruby2xml.make_leaf(poem_0.collection,'recueil'))
    preamble_e.add(Ruby2xml.make_leaf_from_int(poem_0.poem_date,'date'))
    author_e = Element::new('auteur')
    author_e.add(Ruby2xml.make_leaf(poem_0.author_firstname,'prenom'))
    author_e.add(Ruby2xml.make_leaf(poem_0.author_lastname,'nom'))
    author_e.add(Ruby2xml.make_leaf_from_int(poem_0.author_birthdate,
                                             'naissance'))
    author_e.add(Ruby2xml.make_leaf_from_int(poem_0.author_deathdate,'deces'))
    preamble_e.add(author_e)
    preamble_e
  end
  #
  def Ruby2xml.make_leaf(content_s,element_name)
    e = Element::new(element_name)
    e.text = content_s
    e
  end
  #
  def Ruby2xml.make_leaf_from_int(content_i,element_name)
    Ruby2xml.make_leaf(content_i.to_s,element_name)
  end
  #
  def Ruby2xml.make_body(body_s_a)
    body_e = Element::new('corps')
    strophe_e = Element::new('strophe')
    body_s_a.each do |ligne|
    if ligne == ""
    body_e.add(strophe_e)
    strophe_e = Element::new('strophe')
    elsif ligne =~ /\A\t.*/
    ali_e = Element::new('r')
    ligne_e = Element::new('ligne')
    ligne_e.add(ali_e)
    ligne_e.text= ligne
    strophe_e.add(ligne_e)
    else
    strophe_e.add(Ruby2xml.make_leaf(ligne, 'ligne'))
    end
    end
    body_e.add(strophe_e)
    body_e
  end
  #
  def Ruby2xml.ruby2xml(poem_0,output_filename)
    output_encoding_s = 'UTF-8'
    document = Document::new()
    document.add(XMLDecl::new('1.0',output_encoding_s))
    poem_0_dtd_s = poem_0.dtd_s
    document.add(DocType::new([poem_0_dtd_s,'SYSTEM',poem_0_dtd_s + '.dtd']))
    root = Element::new('poemefr0')
    root.add(Ruby2xml.make_preamble(poem_0))
    root.add(Ruby2xml.make_body(poem_0.body))
    document.add(root)
    document.write(File::new(output_filename,'w:' + output_encoding_s),2)
  end
  #
end
