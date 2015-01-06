SAMPLE1 : dialog { 
	label = "B&F Dias - Air grid corte 1"; 
	: column {
		spacer;

			
			: text {
				key = "text1";
				value = "Air grid corte 1"; 
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
                label = "Comprimento";
                edit_width = 15;
                value = "5000";
                initial_focus = true;

            }
			
			: edit_box {
                key = "qtdDifusores";
                label = "Quantidade de difusores";
                edit_width = 15;
                value = "8";
				alignment  = right;
		    }
			
			: edit_box {
                key = "comprimentoLastro";
                label = "Comprimento do lastro";
                edit_width = 15;
                value = "780";
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


