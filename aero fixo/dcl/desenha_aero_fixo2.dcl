SAMPLE1 : dialog { 
	label = "B&F Dias - Sistema de aeração fixo"; 
	: column {
		spacer;

			
			: text {
				key = "text1";
				value = "desenha_aero_fixo2.fas"; 
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
                key = "qtdbf";
                label = "Quantidade de ramais:";
                edit_width = 15;
                value = "10";
                initial_focus = true;
            }
			: edit_box {
                key = "espaco";
                label = "Espaçamento entre eles:";
                edit_width = 15;
                value = "1000";
				alignment  = right;
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


