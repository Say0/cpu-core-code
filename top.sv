module top #(
	parameter INST_MEM_WID = 17,
	parameter DATA_MEM_WID = 32
) (
/*
	input logic CLK_P,
	input logic CLK_N,
*/
//clk -> clk_wiz　を通してから供給
	input logic clk,
	input logic UART_RX,
	output logic UART_TX,
	input logic SW_W,
	input logic SW_E,
	output logic[7:0] LED,
	//bram for data
	output addra[DATA_MEM_WID-1:0],
	output din [31:0],
	input dout [31:0],
	output wea
);
//-clk
//must add:align -> 8 to 32, -> receive_wrapper
//local param
	localparam OP_SPECIAL = 6'b000000;
//	localparam OP_FPU     = 6'b010001;
	localparam OP_ADDI    = 6'b001000;
	localparam OP_ANDI    = 6'b001100;
	localparam OP_ORI     = 6'b001101;
//	localparam OP_SLTI    = 6'b001010;
	localparam OP_BEQ     = 6'b000100;
	localparam OP_BNE     = 6'b000101;
	localparam OP_BGEZ     = 6'b000001;
	localparam OP_BGTZ     = 6'b000111;
	localparam OP_BLEZ     = 6'b000110;
//	localparam OP_BLTZ     = 6'b000001;
	localparam OP_J       = 6'b000010;
	localparam OP_JAL     = 6'b000011;
	localparam OP_LW      = 6'b100011;
	localparam OP_SW      = 6'b101011;
//	localparam OP_FTOI    = 6'b111000;
//	localparam OP_ITOF    = 6'b110000;

	localparam FUNCT_ADD  = 6'b100000;
	localparam FUNCT_SUB  = 6'b100010;
	localparam FUNCT_AND  = 6'b100100;
	localparam FUNCT_OR   = 6'b100101;
//	localparam FUNCT_NOR  = 6'b100111;
//	localparam FUNCT_SLL  = 6'b000000;
//	localparam FUNCT_SRL  = 6'b000010;
//	localparam FUNCT_SLT  = 6'b101010;
	localparam FUNCT_JR   = 6'b001000;
	localparam FUNCT_JALR = 6'b001001;

//	localparam FPU_OP_SPECIAL = 2'b10;
//	localparam FPU_OP_B       = 2'b01;
/*
	localparam FPU_FUNCT_ADD  = 6'b000000;
	localparam FPU_FUNCT_SUB  = 6'b000001;
	localparam FPU_FUNCT_MUL  = 6'b000010;
	localparam FPU_FUNCT_DIV  = 6'b000011;
	localparam FPU_FUNCT_MOV  = 6'b000110;
	localparam FPU_FUNCT_NEG  = 6'b000111;
	localparam FPU_FUNCT_ABS  = 6'b000101;
	localparam FPU_FUNCT_SQRT = 6'b000100;
	localparam FPU_FUNCT_C_EQ = 6'b110010;
	localparam FPU_FUNCT_C_LT = 6'b111100;
	localparam FPU_FUNCT_C_LE = 6'b111110;
*/
/*
	localparam FETCH = 2'b00;
	localparam DECODE = 2'b01;
	localparam EXECUTE = 2'b10;
	localparam WRITE = 2'b11;
*/
	localparam EXECUTE = 1'b0;
	localparam WRITE = 1'b1;

//register
	logic [31:0] gpr[31:0] = {31{32'bx},32'b0};
	logic [31:0] fpr[31:0];
//pc
	logic pc [INST_MEM_WID-1:0] = 17'b0;
	logic pc_add_1 [INST_MEM_WID-1:0];
	assign pc_add_1 = pc +1;
//instr_mem,instr<=receive_wrapper で32bit積めていく仕様
//とりあえずはinstr_memに詰められているとする
	logic [2 ** INST_MEM_WID-1:0] [31:0] instr_mem;
//	logic [INST_MEM_WID-1:0] address; 
	logic [31:0] instr;
	assign instr = instr_mem[pc];
//
	logic [1:0] stage = EXECUTE;
//bram
	assign wea = (instr[31:26] == OP_SW) ? 1 : 0;  
	assign addra  = instr[25:21] + $signed{16{instr[15]},instr[15:0]};
	assign din = gpr[instr[20:16]];
//latency
	logic [0:0] latency1 = 1'b0;

//comp
	always@(posedge clk) begin
/*
//-fetch 
		if(stage == FETCH) begin
			stage <= stage +1;
//-decode <= instr = instr_mem[pc] で代用
		end else if (stage == DECODE) begin
			addra <= pc;
			stage <= stage +1;
		end else*/
 		if (stage == EXECUTE) begin
			case(instr[31:26])
//-exec_alu
				OP_SPECIAL:
					case(instr[5:0])
						FUNCT_ADD:gpr[instr[15:11]] <= gpr[instr[25:21]] + gpr[instr[20:16]];
						FUNCT_SUB:gpr[instr[15:11]] <= gpr[instr[25:21]] - gpr[instr[20:16]];
						FUNCT_AND:gpr[instr[15:11]] <= gpr[instr[25:21]] & gpr[instr[20:16]];
						FUNCT_OR:gpr[instr[15:11]] <= gpr[instr[25:21]] | gpr[instr[20:16]];
						FUNCT_JR:gpr[0] <= gpr[0] | 32'b0;
						FUNCT_JALR:gpr[instr[15:11]]<=pc_add_1;
					endcase
				OP_ADDI:gpr[instr[20:16]] <= gpr[instr[25:21]] + $signed({16{instr[15]},instr[15:0]});
				OP_ANDI:gpr[instr[20:16]] <= gpr[instr[25:21]] & {16'b0,instr[15:0]};
				OP_ORI:gpr[instr[20:16]] <= gpr[instr[25:21]] | {16'b0,instr[15:0]};
				OP_BEQ:gpr[0] <= gpr[0] | 32'b0;
				OP_BNE:gpr[0] <= gpr[0] | 32'b0;
				OP_BGEZ:gpr[0] <= gpr[0] | 32'b0;
				OP_BGTZ:gpr[0] <= gpr[0] | 32'b0;
				OP_BLEZ:gpr[0] <= gpr[0] | 32'b0;
				OP_J:gpr[0] <= gpr[0] | 32'b0;
				OP_JAL:gpr[31] <= pc_add_1;
//-exec_load
				OP_LW:gpr[instr[20:16]] <= dout;
			endcase
			if ((instr[25:21] == OP_LW) || (instr[25:21] == OP_SW)) begin
				stage <= stage + latency1[0];
				latency1 <= latency1 ^ 1'b1;
			end else begin			
				stage <= stage +1;
			end
//stage_renew 
//special_case 1.JALR 2.JR 3.BEQ,BNE,BGEZ,BGTZ,BLEZ 4.J 5,JAL
		end else if (stage == WRITE) begin
			case(instr[31:26])
				OP_JALR:pc <= gpr[instr[25:21]];
				OP_JR:pc <= gpr[instr[25:21]];
				OP_BEQ:begin
					if (instr[20:16] == instr[15:11])	begin
						pc <= pc+$signed{instr[15],instr[15:0]};
					end else begin
						pc <= pc_add_1;
					end
				end
				OP_BNE:
				OP_BGEZ:
				OP_BGTZ:
				OP_BLEZ:
				OP_J:pc <= instr[INST_MEM_WID-1:0];
				OP_JAL:pc <= instr[INST_MEM_WID-1:0];
				default : pc <= pc_add_1;
			endcase
		end
	end
				
				

//communicate
	logic data_rec[7:0];
	logic data_send[7:0];
//	receiver_top receive_top(clk,ready_rec,UART_RX,valid_rec,data_rec);
//neccesary? BRAM for store the instructions ahead of actually using them

	sender	send(clk,valid_send,data_send,ready_send,UART_TX);
