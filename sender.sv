//change tx_busy to ready_send ok
module sender #(CLK_PER_HALF_BIT = 5208)(
	input logic	clk,
	input logic valid_send,
	input logic data_send[7:0],
	output logic ready_send,
	output logic UART_TX
)

   localparam e_clk_bit = CLK_PER_HALF_BIT * 2 - 1;

   localparam e_clk_stop_bit = (CLK_PER_HALF_BIT*2*9)/10 - 1;
   
   logic [7:0]                  txbuf;
   logic [3:0]                  status;
   logic [31:0]                 counter;
   logic                        next;
   logic                        fin_stop_bit;
   logic                        rst_ctr;
   
   localparam s_idle = 0;
   localparam s_start_bit = 1;
   localparam s_bit_0 = 2;
   localparam s_bit_1 = 3;
   localparam s_bit_2 = 4;
   localparam s_bit_3 = 5;
   localparam s_bit_4 = 6;
   localparam s_bit_5 = 7;
   localparam s_bit_6 = 8;
   localparam s_bit_7 = 9;
   localparam s_stop_bit = 10;
   
   // generate event signal
   always @(posedge clk) begin
      /*if (~rstn) begin
         counter <= 0;
         next <= 0;
         fin_stop_bit <=0;
      end else begin*/
         if (counter == e_clk_bit || rst_ctr) begin
            counter <= 0;
         end else begin
            counter <= counter + 1;
         end
         if (~rst_ctr && counter == e_clk_bit) begin
            next <= 1;
         end else begin
            next <= 0;
         end
         if (~rst_ctr && counter == e_clk_stop_bit) begin
            fin_stop_bit <= 1;
         end else begin
            fin_stop_bit <= 0;
         end
      end
   //end

   always @(posedge clk) begin
      /*if (~rstn) begin
         txbuf <= 8'b0;
         status <= s_idle;
         rst_ctr <= 0;
         UART_TX <= 1;
      end else begin*/
         rst_ctr <= 0;
         ready_send <= 0;
         if (status == s_idle) begin
            if (valid_send) begin
               txbuf <= data_send;
               status <= s_start_bit;
               rst_ctr <= 1;
               UART_TX <= 0;
            end
         end else if (status == s_stop_bit) begin
            if (fin_stop_bit) begin
               UART_TX <= 1;
               status <= s_idle;
               ready_send <= 1;
            end
         end else if (next) begin
            if (status == s_bit_7) begin
               UART_TX <= 1;
               status <= s_stop_bit;
            end else begin
               UART_TX <= txbuf[0];
               txbuf <= txbuf >> 1;
               status <= status + 1;
            end
         end
      end
   //end
endmodule // uart_tx
