      WHEN OTHERS   => RETURN "--------";
   END CASE;
END program;

BEGIN

data <= program(conv_integer(addr));

END sim_model;