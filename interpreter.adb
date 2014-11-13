with Lexical_Analyzers, Parsers, Features, Ada.Exceptions, Ada.Text_IO;
use Lexical_Analyzers, Parsers, Features, Ada.Exceptions, Ada.Text_IO;

procedure Interpreter is

   p: Parser;
   f: feature;

begin
   p := create_parser ("test3.e");
   parse (p, f);
   execute (f);
exception
   when e: lexical_exception =>
      put_line (exception_information (e));
   when e: parser_exception =>
      put_line (exception_information (e));
   when others =>
      put_line ("unknown error occurred - terminating");
end Interpreter;
