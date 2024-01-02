--
-- Autorzy / Authors:
-- Jakub Je™drzejczyk (188752)
-- Taras Shuliakevych (196615)
--

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
With Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Store is

   --
   -- Constant variables
   --
   -- Zmienne stale
   --
   
   Product_1: constant String(1 .. 14) := "Shift Register";
   Product_2: constant String(1 .. 8) := "Resistor";
   Product_3: constant String(1 .. 5) := "Diode";
   Product_4: constant String(1 .. 11) := "Condensator";
   Product_5: constant String(1 .. 10) := "Transistor";
   
   Set_1: constant String(1 .. 11) := "Starter Set";
   Set_2: constant String(1 .. 11) := "Complex Set";
   Set_3: constant String(1 .. 10) := "Simple Set";
   
   Number_Of_Products: constant Integer := 5;
   Number_Of_Sets: constant Integer := 3;
   Number_Of_Consumers: constant Integer := 2;

   Production_Min_Time: constant Integer := 1;
   Production_Max_Time: constant Integer := 3;
   Producer_Attempt_Delay: constant Integer := 3;

   Consumption_Min_Time: constant Integer := 12;
   Consumption_Max_Time: constant Integer := 18;
   
   Total_Waiting_Min_Time: constant Integer := 30;
   Total_Waiting_Max_Time: constant Integer := 45;
   
   Next_Attempt_Min_Time: constant Integer := 10;
   Next_Attempt_Max_Time: constant Integer := 20;
   
   Storage_Capacity: constant Integer := 30;

   Time_To_Order: constant Integer := 3;

   --
   -- Subtypes
   --
   -- Subtypy
   --

   subtype Product_Type is Integer range 1 .. Number_Of_Products;
   subtype Set_Type is Integer range 1 .. Number_Of_Sets;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;
   subtype Production_Time_Range is Integer range Production_Min_Time .. 
     Production_Max_Time;
   subtype Consumption_Time_Range is Integer range Consumption_Min_Time .. 
     Consumption_Max_Time;
   subtype Total_Waiting_Range is Integer range Total_Waiting_Min_Time .. 
     Total_Waiting_Max_Time;
   subtype Next_Attempt_Range is Integer range Next_Attempt_Min_Time .. 
     Next_Attempt_Max_Time;

   --
   -- Types
   --
   -- Typy
   --

   type Storage_type is array (Product_Type) of Integer;

   --
   -- Array types
   --
   -- Typy tablicowe
   --

   Product_Name: constant array (Product_Type) of Unbounded_String
      := (To_Unbounded_String(Product_1), To_Unbounded_String(Product_2), To_Unbounded_String(Product_3), To_Unbounded_String(Product_4), To_Unbounded_String(Product_5));
   Set_Name: constant array (Set_Type) of Unbounded_String
     := (To_Unbounded_String(Set_1), To_Unbounded_String(Set_2), To_Unbounded_String(Set_3));
   Set_Content: constant array(Set_Type, Product_Type) of Integer
         := ((2, 2, 2, 2, 2),
            (2, 0, 0, 0, 2),
            (0, 1, 1, 1, 0));
   Consumer_Name: constant array (1 .. Number_Of_Consumers)
         of String(1 .. 6)
         := ("Alicja", "Bogdan");
   Set_Number: array(Set_Type) of Integer
            := (1, 1, 1);
   -- 
   -- Packages
   --
   -- Paczki
   --
   
   package Random_Set is new
      Ada.Numerics.Discrete_Random(Set_Type);
   package Random_Consumption is new
     Ada.Numerics.Discrete_Random(Consumption_Time_Range);
   package Random_Total_Wait is new
     Ada.Numerics.Discrete_Random(Total_Waiting_Range);
   package Random_Next_Attempt is new
     Ada.Numerics.Discrete_Random(Next_Attempt_Range);

   --
   -- Tasks
   -- 
   -- Zadania
   --

   task type Producer is
      entry Start(Product: in Product_Type);
   end Producer;

   task type Consumer is
      entry Start(Consumer_Number: in Consumer_Type);
   end Consumer;

   task type Buffer is
      entry Take(Product: in Product_Type; Number: in Integer; Accepted: out Boolean);
      entry Deliver(Set: in Set_Type; Number: out Integer);
   end Buffer;

   --
   -- Variable tasks
   --
   -- Zmienne zadania
   --

   P: array ( 1 .. Number_Of_Products ) of Producer;
   K: array ( 1 .. Number_Of_Consumers ) of Consumer;
   B: Buffer;

   --
   -- Task bodies
   --
   -- Ciala zadan
   --

   task body Producer is
      package Random_Production is new Ada.Numerics.Discrete_Random(Production_Time_Range);

      Generator: Random_Production.Generator;

      Product_Type_Number: Integer;
      Product_Number: Integer;
      
      Accepted: Boolean := False;

   begin 
      accept Start(Product: in Product_Type) do 
         Random_Production.Reset(Generator);

         Product_Number := 1;
         Product_Type_Number := Product;
      end Start;

      Put_Line("[P] Started producer of " & To_String(Product_Name(Product_Type_Number)));
    
      loop
         delay Duration(Random_Production.Random(Generator));
         Put_Line("[P] Product delivered " & To_String(Product_Name(Product_Type_Number))
                  & " #"  & Integer'Image(Product_Number));
         loop
            B.Take(Product_Type_Number, Product_Number, Accepted);
            if Accepted = False then
               Put_Line("[P] Couldn't deliver " & To_String(Product_Name(Product_Type_Number)));
               delay Duration(Producer_Attempt_Delay);
            else
               Product_Number := Product_Number + 1;
            end if;
            exit;
         end loop;
      end loop;
   end Producer;



   task body Consumer is
      Consumption_Generator: Random_Consumption.Generator;
      Set_Completion_Generator: Random_Set.Generator;
      Next_Attempt_Generator: Random_Next_Attempt.Generator;
      Total_Wait_Generator: Random_Total_Wait.Generator;
      
      Consumer_Nb : Consumer_Type;
      Set_Number : Integer;
      Set_Type : Integer;
   begin
      accept Start (Consumer_Number: in Consumer_Type) do

         Random_Consumption.Reset(Consumption_Generator);
         Random_Set.Reset(Set_Completion_Generator);
         Random_Next_Attempt.Reset(Next_Attempt_Generator);
         Random_Total_Wait.Reset(Total_Wait_Generator);

         Consumer_Nb := Consumer_Number;
      end Start;

      loop
         Put_Line ("[C] Client arrived: " & Consumer_Name(Consumer_Nb));
         delay Duration(Random_Consumption.Random(Consumption_Generator));
         Set_Type := Random_Set.Random(Set_Completion_Generator);
         select
            delay Duration(Random_Total_Wait.Random(Total_Wait_Generator));
            Put_Line("[C] Client " & Consumer_Name(Consumer_Nb) &
                     ": Couldn't find what they wanted and decide to leave.");
         then abort
            Put_Line("[C] Client " & Consumer_Name(Consumer_Nb) & ": Wants " & To_String(Set_Name(Set_Type)));
      
            loop
               B.Deliver(Set_Type, Set_Number);
               if Set_Number = 0 then
                  Put_Line("[C] Client " & Consumer_Name(Consumer_Nb) &
                     ": Is unable to purchase " & To_String(Set_Name(Set_Type)));
                  delay Duration(Random_Next_Attempt.Random(Next_Attempt_Generator));
               else 
                  Put_Line("[C] Client " & Consumer_Name(Consumer_Nb) &
                     ": Bought " & To_String(Set_Name(Set_Type)) & " #" &
                     Integer'Image(Set_Number));
                  exit;
               end if;
            end loop;    
         end select;
         Put_Line ("[C] Client is leaving: " & Consumer_Name(Consumer_Nb));
         delay Duration(Random_Consumption.Random(Consumption_Generator));
      end loop;
   end Consumer;



   task body Buffer is
      Storage: Storage_type
	      := (0, 0, 0, 0, 0);
      Max_Product_Content: array(Product_Type) of Integer;
      
      In_Storage: Integer := 0;
      All_Products: Integer := 0;
      Current_Type: Integer := 0;

      procedure Setup_Variables is
      begin
         for W in Product_Type loop
            Max_Product_Content(W) := 0;
            for Z in Set_Type loop
                 Max_Product_Content(W) := Max_Product_Content(W) + Set_Content(Z, W);
                 All_Products := All_Products + Set_Content(Z, W);
            end loop;
         end loop;

         while All_Products < Storage_Capacity loop
            Max_Product_Content(Current_Type + 1) := Max_Product_Content(Current_Type + 1) + 1;
            All_Products := All_Products + 1;
            Current_Type := (Current_Type + 1) mod Number_Of_Products;
         end loop;
      end Setup_Variables;

   function Can_Accept(Product: Product_Type) return Boolean is 

   begin
         if In_Storage >= Storage_Capacity then
            return False;
         end if;

      if Integer'Max(0, Max_Product_Content(Product) - Storage(Product)) > 0 then
         return True;
      end if;

      return False;
   end Can_Accept;

   function Can_Deliver(Set: Set_Type) return Boolean is
   begin
	   for W in Product_Type loop
         if Storage(W) < Set_Content(Set, W) then
            return False;
         end if;
	   end loop;

	   return True;
   end Can_Deliver;

   procedure Storage_Contents is
      Content_String: Unbounded_String := To_Unbounded_String("[B] Storage contents: ");
   begin
      for W in Product_Type loop
         Append(Content_String, Integer'Image(Storage(W)) & ", ");
      end loop;

      Put_Line(To_String(Content_String));
   end Storage_Contents;

   begin
      Put_Line("[B] Store opened.");
      Setup_Variables;
      loop
         Put_Line("[B] Awaiting orders...");
         select
            accept Deliver(Set: in Set_Type; Number: out Integer) do
               if Can_Deliver(Set) then
                  Put_Line("[B] Delivered Set " & To_String(Set_Name(Set)) & " #" &
                     Integer'Image(Set_Number(Set)));

                  for W in Product_Type loop
                     Storage(W) := Storage(W) - Set_Content(Set, W);
                     In_Storage := In_Storage - Set_Content(Set, W);
                  end loop;

                  Number := Set_Number(Set);
                  Set_Number(Set) := Set_Number(Set) + 1;
                  Storage_Contents;
               else
                  Number := 0;
               end if;
            end Deliver;
         or delay Duration(Time_To_Order);
            Put_Line("[B] No client orders, waiting for a product to move to the storefront.");
            accept Take(Product: in Product_Type; Number: in Integer; Accepted: out Boolean) do
               if Can_Accept(Product) then
                  Put_Line("[B] Product moved to the storefront: " & To_String(Product_Name(Product)) & " (" & Integer'Image(Product) & " )");
                  Storage(Product) := Storage(Product) + 1;
                  Accepted := True;
                  In_Storage := In_Storage + 1;
                  Storage_Contents;
               else
                  Put_Line("[B] Rejected product " & To_String(Product_Name(Product)) & ": No room left.");
                  Accepted := False;
               end if;
            end Take;
         end select;
      end loop;
   end Buffer;
   
begin
   delay 5.0;
   
   for I in 1 .. Number_Of_Products loop
      P(I).Start(I);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      K(J).Start(J);
   end loop;
end Store;
