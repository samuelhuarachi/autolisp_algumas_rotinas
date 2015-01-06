SAMPLE1 : dialog { 
	label = "B&F Dias - Sistema de aeração fixo"; 
	: column {
		spacer;

			
			: text {
				key = "text1";
				value = "desenha aero fixo.fas"; 
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
                value = "10000";
                initial_focus = true;

            }
			
			: edit_box {
                key = "largura";
                label = "Largura:";
                edit_width = 15;
                value = "8000";

				alignment  = right;
		    }
			
			: edit_box {
                key = "qtd";
                label = "Qtd de difusores por ramal:";
                edit_width = 15;
                value = "12";

		    }
			
			: edit_box {
                key = "qtdColunas";
                label = "Quantidade de ramais:";
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


