/**
* Name: BMSTmodel
* Based on the internal empty template. 
* Author: farat
* Tags: 
*/


model BMSTmodel


global {
	file shape_file_hospitals <- file("../includes/soins.shp");
	file shape_file_tram <- file("../includes/tramway.shp");
	file shape_tpg_stops <- file("../includes/tpg_stops.shp");
	geometry shape <- envelope(shape_file_hospitals);
	geometry shape_stops <- envelope(shape_tpg_stops);
	int people_sane <- 0 update: length(people where (each.state = "healthy"));
	int people_contaminated <- 0 update: length(people where (each.state = "infected"));
	int population <- 800 update: people_sane + people_contaminated;
	int time_to_go_to_hospital <- 0;
	int number_concerned <-0;
	float proba_cure <- 0.8;
	float cran <- 900.0;
	list<string> line_cross_hospital <- nil;
	
	graph the_graph;
	
	init {
		create hospital from: shape_file_hospitals{address <- self.location;
													capacity <- 50;}
		create people number:population;
		
		create tpg_stop from: shape_tpg_stops with:[line:string(read ("LIGNE"))]{
			name <- string(read("NOM_ARRET"));
			if length(tpg_stop where (each.name = name)) > 1 {do die;}
			
			list<point> near <- location neighbors_at cran;
			hospital_near <- hospital where (near contains each.location);
			if !empty(hospital_near) and !(line_cross_hospital contains line){
					add line to: line_cross_hospital;}	
		} 
		the_graph <- as_edge_graph(hospital); }
		
		
		
	reflex environment_reaction when:((people_contaminated/population)>= 0.2){
		float ratio <- people_contaminated/population;
		
		loop h over: hospital{
		if flip(0.2){ h.capacity <- h.capacity + 50;}}
		
		if flip(100/cycle){
			list<string> lines <- nil;
			loop t over: tpg_stop{  if !(lines contains t.line){ add t.line to: lines;} }
			
			list<int> number_per_line <- nil;
			loop l over: lines {add length(tpg_stop where(each.line = l)) to: number_per_line;}
				
			int minimum <- min(number_per_line);
			int rang <- number_per_line index_of minimum;
			tpg_stop template <- one_of(tpg_stop where(each.line = (lines at rang)));
			create tpg_stop number:5 { color <- template.color;
				                       line <- template.line;
				                       list<point> near <- location neighbors_at cran;
			    					   hospital_near <- hospital where (near contains each.location); }
			}
			
		if flip(0.2){ loop p over: people{p.cran_transport <- p.cran_transport * 2;}}
			
		if (ratio > 0.6) and flip(0.3) {
			create hospital number: 1 {address <- self.location;
									   capacity <- 100; 
									   
									   list<point> near <- location neighbors_at cran;
			    					   list<tpg_stop> stops <- tpg_stop where (near contains each.location); 
			    					   loop s over: stops {add self to: s.hospital_near;} }  }
		if ratio > 0.75 {loop p over: people {p.go_see_friend <- 0.001;}}
		
		if ratio < 0.3 {loop p over: people {p.go_see_friend <- 0.5;}}
	     }
		
		
  	reflex saving_data{
		save ("cycle :" + cycle + "Population :" + population + "portion infected :" +
			 (people_contaminated/population) + "number_elder :" + length(people where (each.category = "elder")) + "elder_infected :" +
			 length(people where (each.category = "elder" and each.state = "infected")) + "purcentage_elder_infected"+ length(people where (each.category = "elder" and each.state = "infected"))/people_contaminated + "number_adult :" + length(people where (each.category = "adult")) +
			 "adult_infected" + length(people where (each.category = "adult" and each.state = "infected")) + "purcentage_elder_infected"+ length(people where (each.category = "elder" and each.state = "infected"))/people_contaminated 
			 + "average_time_to_go_to_hospital :" + (time_to_go_to_hospital/(number_concerned+0.1))
			 + "number_hospitals :" + length(hospital) + "number_stops :" + length(tpg_stop)) to:"res.txt" rewrite: (cycle = 0)? true : false;
	}
	

	reflex stop_simulation when: (people_sane = 0) or (people_contaminated = 0){
		do pause;
	} 
}





species tpg_stop{
	rgb color;
	string line;
	list<hospital> hospital_near;
	
	reflex sort_stop when:(cycle=0){
		list<point> near <- location neighbors_at (cran + 2000);
		list<hospital> hospital_line <- hospital where (near contains each.location);
		if empty(hospital_line) or !(line_cross_hospital contains self.line){do die;}
		
		else {int rg <- line_cross_hospital index_of self.line;
		   	  color <- rgb(10*(1+rg),10*(1+rg),10*(1+rg));}
	      }
	      
	
	aspect base {
		draw triangle(240.0) color:color;
	}
}






species hospital {
	rgb color <- #green;
	point address;
	int capacity;
	list<people> patients <- nil;
	int number_people <- 0 update: length(patients);
	
	
	reflex healing when: !(empty(patients)){
		loop p over:patients{
			p.energy <- p.energy -0.1;
			if flip(proba_cure){
				p.state <- "healthy";
				p.energy <- 100.0;
				p.get_contaminate <- p.get_contaminate/2.0;
				p.friend <- p;
				p.target <- "friend";
			}   }
		patients <- patients where (each.state = "infeceted");
	}
	
	aspect base{
		draw square(200.0) color: color;
	}
}





species people {
	point home;
	string category;
	string state;
	string target <- nil;
	people friend <- nil;
	int time_with_friend <- 3;
	float energy <- 100.0;
	float step;
	int time <- 0;
	string way <- "walk";
	float get_contaminate;
	float go_see_friend;
	float choose_tram;
	float cran_transport <-10.0;
	list<hospital> hopitaux_proches <- nil;
	list<point> add_hopitaux <- nil;
	list<tpg_stop> stop_near <- nil;
	list<tpg_stop> pathway<- nil;
	
	init {
		home <- self.location;
		
		if flip(0.73) {category <- "adult";
	 	               get_contaminate <- 0.0013;
	 	               go_see_friend <- 0.5;
	 	               choose_tram <- 0.3;
	 	               step <- 100.0;
	 	               state <- "healthy";
	 	               if flip(0.05){state <- "infected";} }
	 	 else {category <- "elder";
	 	               get_contaminate <- 0.016;
	 	               go_see_friend <- 0.1;
	 	               choose_tram <- 0.8;
	 	               step <- 50.0;
	 	               state <- "healthy";
	 	               if flip(0.2){state <- "infected";}}
	}
	
	reflex infection when: (state != "healthy") {
		energy <- energy - 0.1;
		if empty(hospital where (each.location = self.location)){
			if empty(tpg_stop where (each.location = self.location)){
				list people_near <- self neighbors_at 300.0;
				loop n over:people_near {
					if flip(n.get_contaminate) {n.state <- "infected";}  }    }
			else {loop p over:(people where (each.location = self.location)){ 
				if flip(p.get_contaminate) {p.state <- "infected";} } 
				}  }										} 
	
	
	reflex change_target{
		if (state != "healthy") and (target != "hospital") and flip(1.0 - (energy/100.0)){
			target <- "hospital";
			time <- 0;
		}
		else if (target = nil) and empty(people where ((each.friend = self) and (each != self))) and flip(go_see_friend){
			target <- "friend";
			friend <- one_of(people where ((each.target = nil) and (each != self)));
		}
	}

	
	
	reflex go_hospital when: (target = "hospital") and (state != "healthy") and empty(hospital where (self.location = each.address)){
		
		if empty(hopitaux_proches) and (way = "walk"){
			time <- time+1;
			
			if (time > 10) and !empty(stop_near) and flip(choose_tram + time/100.0){
				self.way <- "transport";
				tpg_stop choice_stop <- one_of(stop_near);
				time <- abs(int(self.location distance_to choice_stop.location/step));
				energy <- energy + (energy - time);
				location <- choice_stop.location;
				
				list<tpg_stop> choice_line <- tpg_stop where (each.line = choice_stop.line);
				
				tpg_stop target_stop <- one_of(choice_line where (!empty(each.hospital_near)));
				float d <- abs(choice_stop distance_to target_stop);
			    pathway <- choice_line where (abs(each distance_to target_stop) <= d and abs(each distance_to choice_stop)<=d);
			    remove target_stop from: pathway;
			    add target_stop to: pathway;
		
				}
			
			else {
				list<point> neighbors <- location neighbors_at (time*step);
				hopitaux_proches <- hospital where (neighbors contains each.location ) ;
				stop_near <- tpg_stop where (neighbors contains each.location ) ;
		
				loop h over: hopitaux_proches{
					add h.address to: add_hopitaux; } }
					}
				
		else if (way = "transport" ){
			if (pathway at 0).hospital_near = nil {
				list<point> neighbors <- location neighbors_at (cran_transport*step);
				
				if !(neighbors contains (pathway at 1).location){
					cran_transport <- cran_transport*2;
					time <- time +1;  }
				else { 
					self.location <- (pathway at 1).location;
					remove (pathway at 0) from: pathway;   }      }
			else {
				hopitaux_proches <- (pathway at 0).hospital_near;
				loop h over: hopitaux_proches{
					add h.address to: add_hopitaux;
				way <- "walk"; }
			}  }
 
			
		else {
			list<hospital> can_go_hospital <- hopitaux_proches where (each.number_people < each.capacity);
			if !empty(can_go_hospital){
				hospital choix <- one_of(can_go_hospital);
				time <- time + int(abs((self.location distance_to choix.location)/step));
				time_to_go_to_hospital <- time_to_go_to_hospital + time;
				number_concerned <- number_concerned + 1;
				location <- choix.address;
				add self to:choix.patients;}}
	}
	
	
	
	reflex go_meet_friend when: (friend != nil) and (self.location != friend.home){

		if (way = "walk"){
			time <- time+1;
			list<point> neighbors <- location neighbors_at (time*step);
			stop_near <- tpg_stop where (neighbors contains each.location);
			
			if neighbors contains friend.home {
			    location <- friend.home;
				if friend != self {time_with_friend <- cycle + time_with_friend;}
				else {target <- nil;
					  friend <- nil;  }
				}
			
			
			else if (time > 10) and !empty(stop_near){
				float d_f <- self.location distance_to friend.home;
				
				list<tpg_stop> stop_friend <- tpg_stop where (abs(each.location distance_to friend.home) <= abs(d_f/2.0));
				list<string> s_f <- nil;
				loop t over: stop_friend{
					if !(s_f contains t.line) {add t.line to: s_f;} }
					
				list<tpg_stop> n_stop_near <- tpg_stop where((stop_near contains each) and (s_f contains each.line));
				
				if !empty(n_stop_near) and flip(choose_tram + time/100.0){
				self.way <- "transport";
				energy <- energy + (energy - time);
				stop_near <- n_stop_near;
				tpg_stop choice_stop <- one_of(stop_near);
				time <- abs(int(self.location distance_to choice_stop.location/step));
				location <- choice_stop.location;
				
				list<tpg_stop> choice_line <- tpg_stop where (each.line = choice_stop.line);
				
				tpg_stop target_stop <- one_of(choice_line where (abs(each.location distance_to friend.home) <= abs(d_f/2.0)));
				float d <- abs(choice_stop distance_to target_stop);
			    pathway <- choice_line where (abs(each distance_to target_stop) <= d and abs(each distance_to choice_stop)<=d);
			    remove target_stop from: pathway;
			    add target_stop to: pathway;
		         }  }
			}	
				
		else {
			if length(pathway) > 1{
				list<point> neighbors <- location neighbors_at (cran_transport*step);
				time <- time +1;
				if !(neighbors contains (pathway at 1).location){
					cran_transport <- cran_transport*2;}
				else { 
					self.location <- (pathway at 1).location;
					remove (pathway at 0) from: pathway;   }      }
			else {
				float d <- (pathway at 0).location distance_to friend.home;
				time <- time + int(abs(d/step));
				location <- friend.home;
				if friend != self {time_with_friend <- cycle + time_with_friend;}
				else {target <- nil;
					  friend <- nil; } }  }
	}
	
	
	
	reflex end_playtime when: (friend != nil) and (location = friend.home) and (cycle = time_with_friend){
		time_with_friend <- 3;
		time <- 0;
		friend <- self;
	}
	
	reflex die when: (energy <= 0.0) {
		list<people> friends <- people where(each.friend = self);
		loop p over: friends {p.friend <- p;}
			                  
	    list<hospital> hospitals <- hospital where(each.patients contains self);
	    loop p over: hospitals {remove self from: p.patients;}
	    
		do die;
	}
	
	
 	aspect base {
		if state = "healthy"{
			draw circle(80.0) color: #blue;
		}
		else {
			draw circle(80.0) color: #red;
		}
		
	}
	
} 





experiment infection_tpg type: gui {
	
	parameter "Shapefile for the hospitals:" var: shape_file_hospitals category: "GIS" ;
	parameter "Probability to cure: " var: proba_cure min: 0.0 max: 1.0 category: "Hospital";
	parameter "Number of people: " var: population min: 200 max: 10000 category: "People";
		
	output {
		display city_display type:3d {
			species tpg_stop aspect: base ;
			species hospital aspect: base ;
			species people aspect: base ;
		}
		
		display population_information refresh: every(5#cycle){
			chart "Age_distribution" type: histogram background: #lightgray size: {0.5,0.5} position: {0, 0.5} {
				data "total_elder" value: length(people where (each.category = "elder")) color:#blue;
				data "elder_ill" value: length(people where (each.category = "elder" and each.state = "infected")) color:#blue;
				data "total_adult" value: length(people where (each.category = "adult")) color:#green;
				data "adult_ill" value: length(people where (each.category = "adult" and each.state = "infected")) color:#green;
				}
				 }
		
		monitor "time_to_go_to_hospital" value: time_to_go_to_hospital/(number_concerned+0.1);
		monitor "portion infected" value: people_contaminated/population;
		monitor "Population" value: population;
		monitor "number_hospitals" value: length(hospital);
		monitor "number_stops" value: length(tpg_stop);
		monitor "hospital_line" value: length(line_cross_hospital);
	}
}

/* Insert your model definition here */

