with Lexical_Analyzers, Features, Boolean_Expressions;
use Lexical_Analyzers, Features, Boolean_Expressions;

package Parsers is

   parser_exception: exception;

   type Parser is private;

   function create_parser (file_name: in String) return Parser;
   procedure parse (p: in out Parser; f: out feature);

private
   type Parser is record
      lex: Lexical_Analyzer;
   end record;

end Parsers;
