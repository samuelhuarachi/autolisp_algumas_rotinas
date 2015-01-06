SAMPLE1 : dialog { 
	label = "B&F Dias - Sistema de aeração air grid"; 
	: column {
		spacer;

			
			: text {
				key = "text1";
				value = "Sistema Air Grid"; 
				fixed_width_font = true;
			}
			: image {
				key = "im2";
				height = 0.2;
				width = 1;
				color = 93;
			}
			
		: boxed_column{
			
			: edit_box{
                key = "comprimento";
                label = "Comprimento:";
                edit_width = 15;
                value = "5000";
                initial_focus = true;

            }
			
			: edit_box {
                key = "comLastro";
                label = "Comprimento do lastro:";
                edit_width = 15;
                value = "810";

				alignment  = right;
		    }
			
			: edit_box {
                key = "qtdLastros";
                label = "Quantidade de lastros:";
                edit_width = 15;
                value = "10";

		    }
			
			: edit_box {
                key = "quartCinque";
                label = "40x40 ou 50x50?: [4/5]";
                edit_width = 15;
                value = "5";

		    }
			
			: edit_box {
                key = "larCentral";
                label = "Largura do tubo central:";
                edit_width = 15;
                value = "134.1594813785923";

		    }
			
			: edit_box {
                key = "quantidadeDif";
                label = "Quantidade de difusores";
                edit_width = 15;
                value = "16";

		    }
			spacer;spacer;
        }
		
		: row {
			spacer;spacer;spacer;
			: button {
				key = "accept";
				label = "Iniciar";
				is_default = true;
				
			}
			: button {
				key = "cancel";
				
				label = "Sair";
				is_default = false;
				is_cancel = true;
			}
			
		}	
    }
}


