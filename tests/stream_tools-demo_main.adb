with Ada.Text_IO;
with Stream_Tools.Timed_Input_Streams;
with Stream_Tools.Timed_Output_Streams;
with Stream_Tools.Bufferd_Streams;
with GNAT.Time_Stamp;
with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;
procedure Stream_Tools.Demo_Main is
   procedure P2;
   procedure P2 is
      P          : aliased Stream_Tools.Bufferd_Streams.Bufferd_Stream (8);
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
         Integer'Write (P'Access, 2);
         Short_Short_Integer'Write (P'Access, 2);
         Short_Short_Integer'Write (P'Access, 3);
         Short_Short_Integer'Write (P'Access, 4);
         Short_Short_Integer'Write (P'Access, -1);
         Short_Short_Integer'Write (P'Access, -1);
         Short_Short_Integer'Write (P'Access, -1);
         Short_Short_Integer'Write (P'Access, -1);
         Short_Short_Integer'Write (P'Access, -1);
         Short_Short_Integer'Write (P'Access, -1);
      end T1;
      task body T2 is
         D : Integer;
      begin
         accept Start;
         for I in 1 .. 10 loop
            Ada.Text_IO.Put_Line ("----------------------------------------------------");
            P.Dump;
            Integer'Read (P'Access, D);
            Ada.Text_IO.Put_Line (D'Img);
            delay 0.01;
         end loop;
         Integer'Read (P'Access, D);  Ada.Text_IO.Put_Line (D'Img);
         Integer'Read (P'Access, D);  Ada.Text_IO.Put_Line (D'Img);
         Integer'Read (P'Access, D);  Ada.Text_IO.Put_Line (D'Img);
      end T2;
   begin
      T1.Start;
      T2.Start;
   end P2;
begin
   --   P1;
   P2;
end Stream_Tools.Demo_Main;
