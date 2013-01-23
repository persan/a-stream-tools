with Ada.Text_IO;
with Serialcomunication.Timed_Input_Streams;
with Serialcomunication.Timed_Output_Streams;
with GNAT.Time_Stamp;
with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;
procedure Serialcomunication.Demo_Main is
   Src        : aliased Serialcomunication.Timed_Input_Streams.Timed_Input_Stream (100);
   Tgt        : aliased Serialcomunication.Timed_Output_Streams.Timed_Output_Stream (100);
   Buffers    : constant array (1 .. 4) of access String :=
                  (new String (1 .. 2),
                   new String (1 .. 3),
                   new String (1 .. 4),
                   new String (1 .. 5));

begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);

   Src.Open (Path => "test-data.txt");
   for I of Buffers loop
      String'Read (Src'Access, I.all);
      Ada.Text_IO.Put_Line (GNAT.Time_Stamp.Current_Time & " " & I.all);
   end loop;
   Src.Close;

   Tgt.Create (Path => "test-data2.txt", Output_Base => 2);
   String'Write (Tgt'Access, "--"); delay 0.4;
   String'Write (Tgt'Access, "11"); delay 0.5;
   String'Write (Tgt'Access, "2244"); delay 0.6;
   String'Write (Tgt'Access, "332343"); delay 0.6;
   Tgt.Close;

end Serialcomunication.Demo_Main;
