//remove rstn! ok
module receiver #(CLK_PER_HALF_BIT = 5208)(
	input logic clk,
	//input logic ready_rec,
	input logic UART_RX,
	output logic valid_rec,
	output logic data_rec[7:0]
);

    localparam e_clk_bit = CLK_PER_HALF_BIT * 2 - 1;
    localparam e_clk_bit_half = CLK_PER_HALF_BIT;
    localparam s_idle = 0;  
    localparam s_bit_ready = 1;
    localparam s_bit_0 = 2;
    localparam s_bit_1 = 3;
    localparam s_bit_2 = 4;
    localparam s_bit_3 = 5;
    localparam s_bit_4 = 6;
    localparam s_bit_5 = 7;
    localparam s_bit_6 = 8;
    localparam s_bit_7 = 9;
    localparam s_stop_bit = 10;
    
    //logic t_rxd;
    logic next;
    logic half;
    logic rst;
    logic [31:0]  counter;
    reg [3:0] status = s_idle;
always @(posedge clk) begin
	if (rst) begin
		counter <= 0;
		next <= 0;
		half <= 0;
	end else begin
		if (counter == e_clk_bit_half) begin
			half <= 1;
			counter <= counter + 1;
		end else if (counter == e_clk_bit) begin
			counter <= 0;
			next <= 1;
		end else begin
			counter <= counter + 1;
			half <= 0;
			next <= 0;
		end
   end
end

always @(posedge clk) begin
	valid_rec <= 0;
	rst <= 0;
	ferr <= 0;
/*	if (~rstn) begin
		data_rec <= 0;
		status <= s_idle;
		rst <= 1;
	end else begin*/
	if (status == s_idle && ~UART_RX) begin    
		status <= s_bit_ready;
		rst <= 1;
	end else if (status == s_bit_ready && half) begin
		status <= s_bit_0;
		rst <= 1;
	end else if (next) begin
		if (status == s_stop_bit) begin
			valid_rec <= 1;
			status <= s_idle;
			rst <= 1;
			ferr <= UART_RX ? 0 : 1;
		end else if (status != s_idle)begin
			data_rec <= data_rec >> 1;
			data_rec[7] <= UART_RX;
			status <= status + 1;
			rst <= 1;
		end
	end
end
//end
endmodule




