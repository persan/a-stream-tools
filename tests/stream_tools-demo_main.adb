with Ada.Text_IO;
with Stream_Tools.Timed_Input_Streams;
with Stream_Tools.Timed_Output_Streams;
with Stream_Tools.Bufferd_Streams;
with GNAT.Time_Stamp;
with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;
procedure Stream_Tools.Demo_Main is
   procedure P1;
   procedure P1 is
      Src        : aliased Stream_Tools.Timed_Input_Streams.Timed_Input_Stream (100);
      Tgt        : aliased Stream_Tools.Timed_Output_Streams.Timed_Output_Stream (100);
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
   end P1;
   procedure P2;
   procedure P2 is
      P          : aliased Stream_Tools.Bufferd_Streams.Bufferd_Stream (1000);
      task T1 is
         entry Start;
      end T1;
      task T2 is
         entry Start;
      end T2;

      task body T1 is
      begin
         accept Start;
         for I in 1 .. 10 loop
            Integer'Write (P'Access, I);
            delay 0.4;
         end loop;
         Short_Short_Integer'Write (P'Access, 2);
         Short_Short_Integer'Write (P'Access, 2);
         Short_Short_Integer'Write (P'Access, 3);
         Short_Short_Integer'Write (P'Access, 4);
      end T1;
      task body T2 is
         D : Integer;
      begin
         accept Start;
         for I in 1 .. 10 loop

            Integer'Read (P'Access, D);
            Ada.Text_IO.Put_Line (D'Img);
            delay 0.01;
         end loop;
         Integer'Read (P'Access, D);
         Ada.Text_IO.Put_Line (D'Img);
      end T2;
   begin
      T1.Start;
      T2.Start;
   end P2;
begin
   --   P1;
   P2;
end Stream_Tools.Demo_Main;
