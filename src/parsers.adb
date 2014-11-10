with Statements, Tokens, Expressions, Ada.Text_IO;
use Statements, Tokens, Expressions, Ada.Text_IO;

package body Parsers is

   procedure get_compound(p: in out Parser; com: out Compound);
   procedure match(tok: in Token; tok_type: in Token_Type);
   procedure get_statement(p: in out Parser; s_access: out Statement_Access);
   function is_valid_start(tok: in Token) return Boolean;
   procedure get_assignment_statement(p: in out Parser; s_access: out Statement_Access);
   procedure get_print_statement(p: in out Parser; s_access: out Statement_Access);
   procedure get_expression(p: in out Parser; e_access: out Expression_Access);
   function to_string(lex: in Lexeme) return String;
   procedure get_if_statement(p: in out Parser; s_access: out Statement_Access);
   procedure get_boolean_expression(p: in out Parser; bool_access: out Boolean_Expression);
   procedure get_relational_operator(p: in out Parser; r_oper: out Relational_Operator);
   procedure get_loop_statement(p: in out Parser; s_access: out Statement_Access);
   procedure get_arithmetic_operator(p: in out Parser; a_operator: out Arithmetic_Operator);
   procedure get_token(p: in out Parser; tok_type: in Token_Type);
   -- Create get_id method

   -------------------
   -- create_parser --
   -------------------
   function create_parser (file_name: in String) return Parser is
      par : Parser;
   begin
      par.lex := create_lexical_analyzer(file_name);
      return par;
   end create_parser;

   -----------
   -- parse --
   -----------
   procedure parse (p: in out Parser; f: out feature) is
      tok : Token;
      com : Compound;
   begin
      get_token(p, FEATURE_TOK);
      get_token(p, ID_TOK);
      get_token(p, IS_TOK);
      get_token(p, DO_TOK);
      get_compound(p, com);
      get_next_token(p.lex, tok);
      if(get_token_type(tok) /= END_TOK) then
         raise parser_exception with "garbage at end of program " & to_string(get_lexeme(tok));
      end if;
      f := create_feature(com);
   end parse;

   ------------------
   -- get_compound --
   ------------------
   procedure get_compound(p: in out Parser; com: out Compound) is
      stmt: Statement_Access;
      tok : Token;
   begin
      get_statement(p, stmt);
      add(com, stmt);
      tok := get_lookahead_token(p.lex);
      while is_valid_start(tok) loop
         get_statement(p, stmt);
         add(com, stmt);
         tok := get_lookahead_token(p.lex);
      end loop;
   end get_compound;

   --------------------
   -- is_valid_start --
   --------------------
   function is_valid_start(tok: in Token) return Boolean is
   begin
      return get_token_type(tok) = ID_TOK or else get_token_type(tok) = PRINT_TOK or else get_token_type(tok) = IF_TOK or else get_token_type(tok) = FROM_TOK;
   end is_valid_start;

   -------------------
   -- get_statement --
   -------------------
   procedure get_statement(p: in out Parser; s_access: out Statement_Access) is
      tok : Token;
   begin
      tok := get_lookahead_token(p.lex);
      case get_token_type(tok) is
         when ID_TOK => get_assignment_statement(p, s_access);
         when PRINT_TOK => get_print_statement(p, s_access);
         when IF_TOK => get_if_statement(p, s_access);
         when FROM_TOK => get_loop_statement(p, s_access);
         when others => raise parser_exception with "relational operator expected.";
      end case;
   end get_statement;

   ------------------------
   -- get_loop_statement --
   ------------------------
   procedure get_loop_statement(p: in out Parser; s_access: out Statement_Access) is
      assign_statement : Statement_Access;
      bool_expr : Boolean_Expression;
      comp : Compound;
   begin
      get_token(p, FROM_TOK);
      get_assignment_statement(p, assign_statement);
      get_token(p, UNTIL_TOK);
      get_boolean_expression(p, bool_expr);
      get_token(p, LOOP_TOK);
      get_compound(p, comp);
      get_token(p, END_TOK);
      s_access := create_loop_statement(assign_statement, bool_expr, comp);
   end get_loop_statement;

   ----------------------
   -- get_if_statement --
   ----------------------
   procedure get_if_statement(p: in out Parser; s_access: out Statement_Access) is
      bool_expr : Boolean_Expression;
      comp_1 : Compound;
      comp_2 : Compound;
   begin
      get_token(p, IF_TOK);
      get_boolean_expression(p, bool_expr);
      get_token(p, THEN_TOK);
      get_compound(p, comp_1);
      get_token(p, ELSE_TOK);
      get_compound(p, comp_2);
      get_token(p, END_TOK);
      s_access := create_if_statement(bool_expr, comp_1, comp_2);
   end get_if_statement;

   ----------------------------
   -- get_boolean_expression --
   ----------------------------
   procedure get_boolean_expression(p: in out Parser; bool_access: out Boolean_Expression) is
     rel_operator : Relational_Operator;
     expr_1 : Expression_Access;
     expr_2 : Expression_Access;
   begin
     get_relational_operator(p, rel_operator);
     get_expression(p, expr_1);
     get_expression(p, expr_2);
     bool_access := create_boolean_expression(rel_operator, expr_1, expr_2);
   end get_boolean_expression;

   -----------------------------
   -- get_relational_operator --
   -----------------------------
   procedure get_relational_operator(p: in out Parser; r_oper: out Relational_Operator) is
      tok : Token;
   begin
      get_next_token(p.lex, tok);
      case get_token_type(tok) is
         when NE_TOK => r_oper := NE_OP;
         when EQ_TOK => r_oper := EQ_OP;
         when GT_TOK => r_oper := GT_OP;
         when GE_TOK => r_oper := GE_OP;
         when LT_TOK => r_oper := LT_OP;
         when LE_TOK => r_oper := LE_OP;
         when others => raise parser_exception with "relational operator expected.";
      end case;
   end get_relational_operator;

   ------------------------------
   -- get_assignment_statement --
   ------------------------------
   procedure get_assignment_statement(p: in out Parser; s_access: out Statement_Access) is
      tok : Token;
      var : Id;
      expr : Expression_Access;
   begin
      get_next_token(p.lex, tok);
      var := create_id(get_lexeme(tok)(1));
      get_token(p, ASSIGN_TOK);
      get_expression(p, expr);
      s_access := create_assignment_statement(var, expr);
   end get_assignment_statement;

   -------------------------
   -- get_print_statement --
   -------------------------
   procedure get_print_statement(p: in out Parser; s_access: out Statement_Access) is
      expr : Expression_Access;
   begin
      get_token(p, PRINT_TOK);
      get_token(p, LEFT_PAREN_TOK);
      get_expression(p, expr);
      get_token(p, RIGHT_PAREN_TOK);
      s_access := create_print_statement(expr);
   end get_print_statement;

   --------------------
   -- get_expression --
   --------------------
   procedure get_expression(p: in out Parser; e_access: out Expression_Access) is
      tok : Token;
      a_operator : Arithmetic_Operator;
      expr_1 : Expression_Access;
      expr_2 : Expression_Access;
   begin
      tok := get_lookahead_token(p.lex);
      case get_token_type(tok) is
         when ID_TOK =>
            get_next_token(p.lex, tok);
            e_access := create_variable_expression(get_lexeme(tok)(1));
         when LIT_INT_TOK =>
            get_next_token(p.lex, tok);
            e_access := create_constant_expression(Integer'Value (to_string(get_lexeme(tok))));
         when others =>
            get_arithmetic_operator(p, a_operator);
            get_expression(p, expr_1);
            get_expression(p, expr_2);
            e_access := create_binary_expression(a_operator, expr_1, expr_2);
      end case;
   end get_expression;

   -----------------------------
   -- get_arithmetic_operator --
   -----------------------------
   procedure get_arithmetic_operator(p: in out Parser; a_operator: out Arithmetic_Operator) is
      tok : Token;
   begin
      get_next_token(p.lex, tok);
      case get_token_type(tok) is
         when ADD_TOK => a_operator := ADD_OP;
         when SUB_TOK => a_operator := SUB_OP;
         when MUL_TOK => a_operator := MUL_OP;
         when DIV_TOK => a_operator := DIV_OP;
         when others => raise parser_exception with "relational operator expected.";
      end case;
   end get_arithmetic_operator;

   ---------------
   -- to_string --
   ---------------
   function to_string(lex: in Lexeme) return String is
      size : Positive := lexeme_length(lex);
      Str : String(1 .. size);
   begin
      for i in Integer range 1 .. size loop
         Str(i) := lex(i);
      end loop;
      return str;
   end to_string;

   -----------
   -- match --
   -----------
   procedure match(tok: in Token; tok_type: in Token_Type) is
   begin
      if get_token_type(tok) /= tok_type then
         raise parser_exception with Token_Type'Image(tok_type) & " expected at row " & Positive'Image(get_row_number(tok)) & " and column " & Positive'Image(get_column_number(tok));
      end if;
   end match;

   ---------------
   -- get_token --
   ---------------
   procedure get_token(p: in out Parser; tok_type: in Token_Type) is
      tok : Token;
   begin
      get_next_token(p.lex, tok);
      match(tok, tok_type);
   end get_token;

end Parsers;
