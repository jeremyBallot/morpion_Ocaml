
(*Creer la cellule, sachant que chaque couleur représente un joueur, et le blanc signifie que la cellule est vide*)
module L =List;;
module A=Array;;

#load"graphics.cma";;
#load "nums.cma";;

open Graphics;;


let taille_grille = ref 8
let taille_cell = ref (700 / !taille_grille)
let taille_gaine = ref 5 
let profondeur = ref 2
let meilleurX =ref 0;;
let meilleurY= ref 0;;
let beta =ref 0;;
let alpha= ref 0;;


type celulle = 
   Rond
  |Croix
  |Vide  ;;
  
(* type grille  = (celulle array) array;; *)

type point = (int * int)

type liste_point =  point list

(*Fonction qui initialise une grille*)

let new_table = Array.make_matrix !taille_grille !taille_grille Vide;;
(*Creation d'une grille vide*)
let grille = ref (Array.make_matrix !taille_grille !taille_grille Vide);;

(* Méthode pour dessiner les Rond  *)

let dessin_cellule x =
open_graph " 512x512"; clear_graph();
let k=512/x in 
for i = 0 to x do
  for j = 0 to x do
    Graphics.draw_circle (i * k + (k/2))(j * k + (k/2))((k / 2) - 5)
  done
done;;

(* Méthode pour dessiner les croix  *)

let des_croix i j k =
 Graphics.moveto ((i*k)+10)((j*k)+10);
 Graphics.lineto ((i*k+k)-10) ((j*k + (k))-10);
 Graphics.moveto ((i*k)+10)((j*k+k)-10);
 Graphics.lineto ((i*k+k)-10)((j*k)+10);
  ;;

(*  Dessiner la grille  *)

let dessin x grille =
open_graph " 700x700"; clear_graph();
let k=700/x in
for i = 0 to x-1 do
  for j = 0 to x-1 do
  Graphics.set_line_width 3;
  Graphics.set_color (Graphics.rgb 255 255 255);
  Graphics.draw_rect(i*k)(j*k)(i+k)(j+k);
  Graphics.set_color (Graphics.rgb 0 70 0);
  Graphics.fill_rect(i*k+1)(j*k+1)(k-2)(k-2);
  match grille.(i).(j) with
    |Croix-> 
      Graphics.set_color (Graphics.rgb 150 150 150);
      Graphics.set_line_width 5;
      des_croix i j k;
      Graphics.set_line_width 1;
    |Rond -> 
      Graphics.set_line_width 5;
      Graphics.set_color (Graphics.rgb 0 150 150);
      Graphics.draw_circle (i * k + (k/2))(j*k + (k/2))((k / 2) - 5)
    |Vide-> Graphics.moveto 0 0;
  done
done;;

(* Vérifie si la case est vide  *)

let coup_legal x y grille =
 if grille.(x).(y) <> Vide then false
 else true;;

(*  Mettre à jour la cellule joué  *)


    
  
 
(* Fonction verifiant si la grille est plaine *)
let est_fin grille  =
let fini= ref true in 
  for i=0 to (Array.length grille) -1 do 
    for j=0 to (Array.length grille.(i))-1 do 
      if grille.(j).(i)= Vide then fini:= false 
    done;
   done;
   !fini;;
   
let deb x = max 0 (min x (!taille_grille-1));;
let plus x = x+1;;

let rec init_bornes (a,b) x y bid = 
	if (x+a) < 0  || (y+b) < 0 || (x+a) > !taille_grille-1 || (y+b) > !taille_grille-1 || bid = 0  then 
		(x::[])@(y::[])
	else init_bornes (a,b) (x+a) (y+b) (bid-1);;



let rec verif_line (a,b) i j k l signe grille liste =
	if  (i <> (k+a)) && (j<> (l+a) && (j >=0 ) && (i >=0 ) && (j < !taille_grille) && (i< !taille_grille)) then 
		if (grille.(i).(j) = signe) then 
			verif_line (a,b) (i+a) (j+b) k l signe grille ((i,j)::liste)
		else []
	else liste ;;


  
let rec count_line (a,b) x y  x' y'  signe nbr grille=
	match (a,b) with
	|(0,_)->if  (y <> (y'+b) && (x >=0 ) && (y >=0 ) && (x < !taille_grille) && (y < !taille_grille)) then
	     			if (grille.(x).(y) = signe) then
					count_line (a,b) (x+a) (y+b)  x' y' signe (nbr+1) grille
	        		else nbr
			else nbr

	|(_,0)-> if((x <> (x'+a)) && (x >=0 ) && (y >=0 ) && (x < !taille_grille) && (y < !taille_grille)) then
	      			if (grille.(x).(y) = signe) then
					count_line (a,b) (x+a) (y+b)  x' y' signe (nbr+1) grille
	      			else nbr
			else nbr
  		
	|(_,_)->if(y<>(y'+b) && (x<> (x'+a)) && (x >=0 ) && (y >=0 ) && (x < !taille_grille) && (y < !taille_grille)) then
	      			if (grille.(x).(y) = signe) then
					count_line (a,b) (x+a) (y+b)  x' y' signe (nbr+1) grille
	      			else nbr
		else nbr	      
	;;


  let cherche_ligne (a,b,c,d) x y grille =
  	let signe = grille.(x).(y) in 
  	let m =  ( init_bornes (a,b) x y  !taille_grille) in 
  	let i = ref (List.hd m) in 
  	let j = ref (List.hd(List.tl m)) in 
  	let n =  ( init_bornes (c,d) !i !j  !taille_grille) in
  	let k = ref (List.hd n) in 
  	let l= ref (List.hd(List.tl n)) in 
 		
	if ((!j >=0 ) && (!i >=0 ) && (!j < !taille_grille) && (!i< !taille_grille)) then 
		let gain = (count_line (a,b) (x+a) (y+b) !i !j signe 0 grille) +
		   (count_line (c,d) (x+c) (y+d) !k !l signe 0 grille) + 1 in 
		
		if gain < !taille_gaine then false
		else true
	else false ;;

let verif_gain x y grille =
	let ok = ref false in 
	if grille.(x).(y)=Vide then 
	begin 
	ok := false;
	!ok
	end
	else
	let directions = [ (*definition des direction de recherche*)
	(-1,-1, 1,1); (-1,0,1,0); (-1,1,1,-1);
	(0 ,-1,0,1) ] in
(List.iter (fun (a,b,c,d)-> if (cherche_ligne (a,b,c,d) x y grille) && !ok = false then ok:= true
			
 ) directions );
!ok;;


  let coups_jouable grille =
  let liste_cell = ref [] in
  for i=0 to (A.length grille)-1 do 
  for j=0 to (A.length grille.(0))-1 do 
    if (coup_legal i j grille) then 
      liste_cell := (i,j)::!liste_cell
   done;
   done;
   !liste_cell;;
  
  
   let maj_cell x y joueur grille=
    grille.(x).(y)<-joueur;
   ;;




let copie_grille grille =
ref (Array.init !taille_grille (fun y ->
Array.init !taille_grille (fun x -> grille.(y).(x)) ));;


let simule_coup x y signe grille=
	let grille_simule = (copie_grille grille) in 
		if (coup_legal x y !grille_simule) then 
			begin 
			!grille_simule.(x).(y)<-signe;
			!grille_simule
			end
		else !grille_simule ;;


let rec machine_rdm grille =
let x =Random.int (A.length grille) in 
let y= Random.int (A.length grille.(0) ) in 
if (coup_legal x y grille) then 
  maj_cell x y Croix grille
else machine_rdm grille ;;


let rec joueur grille =
    let clic = wait_next_event [Button_down] in
    let x= (clic.mouse_x /(!taille_cell)) and y = (clic.mouse_y/(!taille_cell)) in
    if (coup_legal x y grille) then
      maj_cell x y Rond grille
    else 
    joueur grille;;
    

let evalu x y signe grille=
	let eval = ref 0 in
		if(verif_gain x y grille) then
		 begin
		 	if ( signe ==Croix) then
			eval := ! eval-10
			else if (signe == Rond) then 
			eval := ! eval+10
		end;
			
			
	!eval;;
(*faire un test pour lui dire que les combinaison a 5 au milieu sont plus importante que sur le bord*)


let rec minMax prof x y signe gril =
	let maxi = ref (-500) in
	let mini = ref (500) in
	let variable_temp = ref 0 in
	

	if ( prof == 0 || verif_gain x y gril ) then 
		begin
		if ( verif_gain x y gril) then
			begin
			meilleurX :=x;
			meilleurY :=y;
			evalu x y signe gril;
			end
		else
		evalu x y signe gril;
		end
		
	else if(prof==1 || prof== 3 ||prof == 5) then
	begin
		let liste_possible = coups_jouable gril in
		
		try
		
			(List.iter ( fun (a,b)->
				
				 (*let grille_test = ref (simule_coup a b Rond gril) in		
				
				variable_temp:= minMax (prof-1) a b Croix !grille_test;	
				Printf.printf" MINMAX  --> (%d,%d,%d) \n"a b !variable_temp;*)
				gril.(a).(b)<-Rond;
				variable_temp:= minMax (prof-1) a b Croix gril;
				gril.(a).(b)<-Vide;

				if !mini > !variable_temp then 
				
				mini :=!variable_temp; (*Printf.printf" MINMAX resultat  --> (%d) \n" !mini;*)
			
			))liste_possible;
			!mini;
			with | _ -> !mini;
	end
	else begin
		let liste_possible = coups_jouable gril in
		try
		
			(List.iter ( fun (a,b)->
				 (*let grille_t = ref (simule_coup a b Croix gril) in
				variable_temp:= minMax (prof-1) a b Rond !grille_t;
				Printf.printf" a,b, temp --> (%d,%d,%d) \n"a b !variable_temp;*)
				gril.(a).(b)<-Croix;
				variable_temp:= minMax (prof-1) a b Rond gril;
				gril.(a).(b)<-Vide;
				if !maxi < !variable_temp then maxi :=!variable_temp;
			)
		)liste_possible;
		!maxi;
		with | _ -> !maxi;
	end;;
	



let ordIA grille  = 

		let max = ref (-1000)
		and temp = ref (0) in
		let liste_possible = coups_jouable grille in
			
			(List.iter ( fun (a,b)->
			(*let grille_t = ref (simule_coup a b Croix grille) in
			temp :=minMax 3 a b Rond !grille_t;
		Printf.printf" a,b, temp --> (%d,%d,%d) \n"a b !temp;*)
			grille.(a).(b)<-Croix;
			temp:= minMax 1 a b Rond grille;
			grille.(a).(b)<-Vide;
			if !max < !temp then
			begin
			 max  :=!temp;
			meilleurX :=a;
			meilleurY :=b;
			
			(*Printf.printf" a,b, meilleurX, meilleurY --> (%d,%d,%d,%d) \n"a b !meilleurX !meilleurY*)
			end			
			(*else 
			(*Printf.printf" rien trouver et temp --> (%d,%d,%d) \n"a b !temp*)
			*)
		)
		)liste_possible;
maj_cell !meilleurX !meilleurY Croix grille;;



let evalu2 x y grille=
	let eval = ref 0 in
		if(verif_gain x y grille) then
		 begin
		 	if ( grille.(x).(y) ==Croix) then
			eval := ! eval-10
			else if (grille.(x).(y) == Rond) then 
			eval := ! eval+10
		end;
!eval;;


(*fausse fct maxi car mini appel maxi et inversement*)
let maxi x y prof (alpha:int ref) (beta:int ref) grille =
let temp = ref 0 in
	if prof==0 then
temp := 0;
!temp;;

let mini x y prof (alpha:int ref) (beta:int ref) grille =
	let temp = ref 0 in
	if ( prof == 0 || verif_gain x y grille) then
		evalu2 x y grille
	else 
	begin
		let liste_possible = coups_jouable grille in
		try
			(List.iter  (fun (a,b)->
				grille.(a).(b)<- Croix;
				temp := maxi a b (prof-1) alpha beta grille;
				grille.(a).(b)<-Vide;
				if !beta > !temp then beta := !temp;
				if !beta <= !alpha then raise Exit;
				)
			)liste_possible;
			!beta;
			with | _ -> !beta;
	end;;
	
let maxi x y prof (alpha:int ref) (beta:int ref) grille =
	let temp = ref 0 in
	if ( prof == 0 || verif_gain x y grille) then
		evalu2 x y grille
	else 
	begin
		let liste_possible = coups_jouable grille in

		try
		
			(List.iter  (fun (a,b)->
				
				grille.(a).(b)<- Rond;
				temp := mini a b (prof-1) alpha beta grille;
				grille.(a).(b)<-Vide;
				if !alpha > !temp then beta := !temp;
				if !alpha <= !alpha then raise Exit;
				)
			)liste_possible;
			!beta;
			with | _ -> !beta;
	end;;

let calcAphaBeta grille =
		let alpha = ref (-100)
		and beta = ref 100
		and temp = ref (0) in
		let alpha2 = ref (100) 
		and beta2 = ref(-100) in
		let liste_possible = coups_jouable grille in
			
			(List.iter ( fun (a,b)->
			
			grille.(a).(b)<-Rond;
					
					temp := mini a b (1) alpha2  beta2 grille;
					if !alpha< !temp then
					
						alpha :=!temp;
						meilleurX := a;
						meilleurY := b;
				
					grille.(a).(b)<-Vide;
				)
				)liste_possible;
			
maj_cell !meilleurX !meilleurY Croix grille;;

(*maj_cell 1 4 Rond !grille;;
maj_cell 2 5 Rond !grille;;
maj_cell 3 6 Rond !grille;;
maj_cell 4 7 Rond !grille;;*)


(*
maj_cell 1 3 Croix !grille;;
 maj_cell 2 4 Croix !grille;;
 maj_cell 3 5 Croix !grille;;
 maj_cell 4 6 Croix !grille;;*)



   
let jouer()=
while not (est_fin !grille) do 
 begin
   dessin !taille_grille !grille;
   joueur !grille;
   ordIA !grille;
 end;
  done;
  dessin !taille_grille !grille;
  ;;
   
jouer();; 
  



(*
 let rec verif_direct (dx,dy) x y s niv ok grille=
 if (niv <> 5) && ((dx+x) < !taille_grille) && ((dy+y) < !taille_grille) then 
  if grille.(x).(y) = s then  verif_direct (dx,dy) (dx+x) (dy+y) s (niv+1) true grille
  else verif_direct (dx,dy) (dx+x) (dy+y) s (niv+1) false grille
 if ok = false then false 
 else true ;;
   
let verif_gain x y grille =
  let directions = [ (*definition des direction de recherche*)
(-1, -1); (-1, 0); (-1, 1);
(0 , -1); (* X *) (0 , 1);
(1 , -1); (1 , 0); (1 , 1)
]
and signe = grille.(x).(y) in (*signe de la case x y*)
(L.iter 
(fun (xd,yd) (-> 
if (verif_direct (xd,yd) x y signe 0 true grille ) then 
Graphics.set_color (Graphics.rgb 255 0 0);
Graphics.moveto (x * !taille_cell) (y * !taille_cell);
Graphics.lineto ((x+5) * !taille_cell) ((y+5) * !taille_cell);
 ) directions );;*)

(*let verif_ligne (a,b,c,d) x y signe grille =
let win = ref true in

let i= ref (x+(a*5)) in 
let j= ref (y+(c*5)) in
let k= ref (x+(b*5)) in 
let l= ref (y+(d*5)) in
 
if ( !i > !taille_grille) then i:=  !taille_grille 
else if (!i < 0 )then i:=0 ;

if ( !j > !taille_grille) then j:=  !taille_grille 
else if (!j < 0 )then j:=0 ;

if ( !k > !taille_grille) then k:= !taille_grille  
else if (!k < 0 )then k:=0 ;

if ( !l > !taille_grille) then l:=  !taille_grille 
else if (!l < 0 )then l:=0 ;

Printf.printf" borne i  %d \n" !i;
Printf.printf" borne j %d \n" !j;
Printf.printf" borne k %d \n" !k;		
Printf.printf" borne l %d \n" !l;

while( ( !i <= !k ) && ( !j <= !l ) ) do 
    if grille.( !i ).( !j) <> signe then win:= false ;
    i:= !i +1 ;
    j:= !j +1 ;
    Printf.printf" borne i dans while  %d\n " !i;
    Printf.printf" borne j dans while %d \n" !j;
done;
!win;;*)

  
  (*
  
  let rec verif_ligne2 (a,b,c,d) x y grille =
  let signe = grille.(x).(y) in 
    let i =test_borne((a*5)+x-1) in 
    let j =test_borne((b*5)+y-1) in 
    let k =test_borne((c*5)+x-1) in
    let l =test_borne ((d*5)+y-1) in 
    
    while 
       
      
    
    
    if grille.(i).(j) = signe && grille.(k).(l) = signe then 
      verif_ligne2 (a,b,c,d) x y (niveau+1) true grille
    else 
      verif_ligne2 (a,b,c,d) x y (niveau+1) false grille;
  end 
  else
  ok;;*)
  
(*  
let verif_gain x y grille =
let directions = [ (*definition des direction de recherche*)
(-1,-1, 1 , 1 ); (-1, 0,1, 0); (-1, 1, 1 ,-1);
(0 , -1,0 , 1) ] in (*signe de la case x y*)
(L.iter 
(fun (a,b,c,d)-> 
Printf.printf" (%d,%d,%d,%d) x=%d, y=%d" a b c d x y ;
if (verif_ligne2 (a,b,c,d) x y 1 true grille ) then 
Printf.printf" alligne trouvé  "
else Printf.printf" Rien Trouvé \n  "
 ) directions );;*)



    
(*
		begin 
 		let resulte = verif_line (c,d) !i !j !k !l signe grille [] in 
		
		Printf.printf "Maj i,j (%d,%d)  eb\n" !i !j;
		Printf.printf "Maj k,l (%d,%d) eb\n" !k !l;
  		 
			if  decal<10  then 
				if ((L.length resulte) < 5) then 
				cherche_ligne signe (a,b,c,d) (x+c) (y+d) (decal+1)  grille
				else resulte
			else [] 
		end 
	else 
	[];;

cherche_ligne Rond (-1,1,1,-1)  4 4  !grille;;
cherche_ligne Rond (-1,-1,1,1)  2 2  !grille;;
*)

  (*
   if ( ( ((!i <> (!k+c)) && (!j <> (!l+d) ) ) && ( ((!i < 7) && (!i > -1)) && ((!j < 7) && (!j> -1))) ) && (!gain < 5)) then 
   begin 
(*   for g=0 to 4 do  *)
    if grille.(!i).(!j) <> signe then 
	  point_gain := []
    else
      begin
	point_gain := (!i,!j)::[] @ !point_gain;
	gain := plus !gain;
	end;
      begin
       	i := (!i)+c; 
       	j := (!j)+d;
        Printf.printf "Maj i %d\n" !i;
        Printf.printf "Maj j %d\n" !j;
      end;
   end;
   done; 
      
  if !gain < 5 && decal < 5 then
    cherche_ligne signe (a,b,c,d) (x+c) (y+d) (decal+1)  grille
  else  
    !point_gain;;
 
 *)

(*
let verif_gain x y grille =
  let directions = [ (*definition des direction de recherche*)
(-1, -1, 1, 1); (-1, 0, 1, 0); (-1, 1,1,-1);
(0 , -1, 0, 1) ]
and signe = grille.(x).(y) in (*signe de la case x y*)
(L.iter 
(function (a,b,c,d) -> let l = cherche_ligne signe (a,b,c,d) x y 0  grille in 
    match l with 
    |[]-> false
    |(t,j)::q-> true
 ) directions );;
 *)


(*  cherche_ligne (a+c,b+d,c,d) x y decal grille *)
  
 (* let test grille=
  for i=0 to (A.length grille)-1 do 
  for j=0 to (A.length grille.(0))-1 do
    grille.(i).(j)<- Croix
    done;
    done;;

(*if  (x <> (x'+a)) && (y <> (y'+b) && (x >=0 ) && (y >=0 ) && (x < 8) && (y < 8)) then*)

     (*
let rec count_line (a,b) x y  x' y'  signe nbr grille=
	if (a=0) && (b <> 0) then 
		begin 
		if  (y <> (y'+b) && (x >=0 ) && (y >=0 ) && (x < 8) && (y < 8)) then
	     		if (grille.(x).(y) = signe) then
			count_line (a,b) (x+a) (y+b)  x' y' signe (nbr+1) grille
	        	else nbr
		else nbr
		 end
	else if (b=0) && (a <> 0) then 
		begin 
		if  (x <> (x'+a)) && (x >=0 ) && (y >=0 ) && (x < 8) && (y < 8)) then
	      		if (grille.(x).(y) = signe) then
				count_line (a,b) (x+a) (y+b)  x' y' signe (nbr+1) grille
	      		else nbr
		else nbr
  		end
	else if 
		begin 
		if  (x <> (x'+a)) && (x >=0 ) && (y >=0 ) && (x < 8) && (y < 8)) then
	      		if (grille.(x).(y) = signe) then
			count_line (a,b) (x+a) (y+b)  x' y' signe (nbr+1) grille
	      		else nbr
		else nbr
		end;;
		*)

  *)






