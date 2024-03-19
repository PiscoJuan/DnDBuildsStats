tirar_dado <- function( ndice,nside){
  sample(1:nside, ndice, replace = TRUE)
}

tirar_dado(3,8)

great_weapon_fighting <- function(vector, tamano){
  
  n <- 0
  vector2 <-vector
  for(value in vector){
    n <- n+1
    if(value<=2){
      vector2 <- replace(vector2, n,tirar_dado(1,tamano))
    }
  }
  vector2
}

smite <- function(paladinlvl, fiendUndead, crit){
  if (paladinlvl>4){
    paladinlvl <- 4
  }
  if(crit){
    if(fiendUndead){
      tirar_dado(4+(2*(paladinlvl)),8)
    }else{
      tirar_dado(2+(2*(paladinlvl)),8)
    }
  }else{
    if(fiendUndead){
      tirar_dado(2+paladinlvl,8)
    }else{
      tirar_dado(1+paladinlvl,8)
    }
  }
}

improved_smite <- function(crit){
  if(crit){
    tirar_dado(2,8)
  }else{
    tirar_dado(1,8)
  }
}

sneak_attack <- function(roguelvl, crit){
  if(crit){
    tirar_dado((2*ceiling(roguelvl/2)), 6)
  }else{
    tirar_dado(ceiling(roguelvl/2), 6)
  }
  
}

dread_ambusher <- function(crit){
  if(crit){
    tirar_dado(2,8)
  }else{
    tirar_dado(1, 8)
  }
}

great_axe <- function(crit, orc){
  if(crit){
    if(orc){
      great_weapon_fighting(tirar_dado(3,12), 12)
    }else{
      great_weapon_fighting(tirar_dado(2,12), 12)
    }
  }else{
    great_weapon_fighting(tirar_dado(1,12), 12)
  }
}

stirring_dragon_great_axe <- function(crit, orc){
  if(crit){
    if(orc){
      x <- great_weapon_fighting(tirar_dado(3,12), 12)
      x <- append(x,great_weapon_fighting(tirar_dado(2,6), 6))
    }else{
      x <- great_weapon_fighting(tirar_dado(2,12), 12)
      x <- append(x,great_weapon_fighting(tirar_dado(2,6), 6))
    }
  }else{
    x <- great_weapon_fighting(tirar_dado(1,12), 12)
    x <- append(x,great_weapon_fighting(tirar_dado(1,6), 6))
  }
  x<-append(x,1)
  return(x)
}
wakened_dragon_great_axe <- function(crit, orc){
  if(crit){
    if(orc){
      x <- great_weapon_fighting(tirar_dado(3,12), 12)
      x <- append(x,great_weapon_fighting(tirar_dado(4,6), 6))
    }else{
      x <- great_weapon_fighting(tirar_dado(2,12), 12)
      x <- append(x,great_weapon_fighting(tirar_dado(4,6), 6))
    }
  }else{
    x <- great_weapon_fighting(tirar_dado(1,12), 12)
    x <- append(x,great_weapon_fighting(tirar_dado(2,6), 6))
  }
  x<-append(x,2)
  return(x)
}
ascendant_dragon_great_axe <- function(crit, orc){
  if(crit){
    if(orc){
      x <- great_weapon_fighting(tirar_dado(3,12), 12)
      x <- append(x,great_weapon_fighting(tirar_dado(6,6), 6))
    }else{
      x <- great_weapon_fighting(tirar_dado(2,12), 12)
      x <- append(x,great_weapon_fighting(tirar_dado(6,6), 6))
    }
  }else{
    x <- great_weapon_fighting(tirar_dado(1,12), 12)
    x <- append(x,great_weapon_fighting(tirar_dado(3,6), 6))
  }
  x<-append(x,3)
  return(x)
}

orc_fury <- tirar_dado(1,12)[1]
orc_fury
great_weapon_fighting(tirar_dado(8,6), 6)

smite(8, TRUE, TRUE)
improved_smite(TRUE)
sneak_attack(3, TRUE)
dread_ambusher(TRUE)
great_axe(TRUE, TRUE)
stirring_dragon_great_axe(TRUE, TRUE)
wakened_dragon_great_axe(TRUE, TRUE)
ascendant_dragon_great_axe(TRUE, TRUE)
#Paladin 12, Rogue 3, Ranger 3, Fighter 2
#Feats: Ability Score Imporvement (+2Str), Orckish Fury (+1Str), GWM
#Stats: Str20, Dex13, Con10, Int8, Wis13, Cha14

turno1_no_haste <-function(armau, resFisico, resRadiant, fiendUndead){
  arma <- 5*sum(armau)
  if(resFisico){
    arma <- arma/2
  }
  impsmite <- 5*sum(improved_smite(TRUE))
  smite4 <- 3*sum(smite(4, fiendUndead, TRUE))
  smite3 <- 2*sum(smite(3, fiendUndead, TRUE))
  divinef <- sum(tirar_dado(10,4))
  if(resRadiant){
    impsmite <- impsmite/2
    smite4 <- smite4/2
    smite3 <- smite3/2
    divinef <- divinef/2
  }
  sneak <- sum(sneak_attack(3, TRUE))
  dread <- sum(dread_ambusher(TRUE))
  orcf <- tirar_dado(1,12)[1]
  GWM <- 10
  Str <- 5
  cha <- 7
  flat <- ((GWM*5)+(Str*5)+(cha*5))
  total <- arma+impsmite+smite4+smite3+divinef+sneak+dread+orcf+flat
  return(total)
} 

turno1_haste <-function(armau, resFisico, resRadiant, fiendUndead){
  arma <- 6*sum(armau)
  if(resFisico){
    arma <- arma/2
  }
  impsmite <- 6*sum(improved_smite(TRUE))
  smite4 <- 3*sum(smite(4, fiendUndead, TRUE))
  smite3 <- 3*sum(smite(3, fiendUndead, TRUE))
  divinef <- sum(tirar_dado(12,4))
  if(resRadiant){
    impsmite <- impsmite/2
    smite4 <- smite4/2
    smite3 <- smite3/2
    divinef <- divinef/2
  }
  sneak <- sum(sneak_attack(3, TRUE))
  dread <- sum(dread_ambusher(TRUE))
  orcf <- tirar_dado(1,12)[1]
  GWM <- 10
  Str <- 5
  cha <- 7
  flat <- ((GWM*6)+(Str*6)+(cha*6))
  total <- arma+impsmite+smite4+smite3+divinef+sneak+dread+orcf+flat
  return(total)
} 
###GREAT AXE
##SIN HASTE
  #Normal
  mean(replicate(10000, sum(turno1_no_haste(great_axe(TRUE, TRUE), FALSE, FALSE, FALSE)))) #527
  #vs Zariel
  mean(replicate(10000, sum(turno1_no_haste(great_axe(TRUE, TRUE), TRUE, TRUE, TRUE)))) #355
  #vs Zariel con arma luz luna
  mean(replicate(10000, sum(turno1_no_haste(great_axe(TRUE, TRUE), FALSE, TRUE, TRUE)))) #410

##CON HASTE
  #Normal
  mean(replicate(10000, sum(turno1_haste(great_axe(TRUE, TRUE), FALSE, FALSE, FALSE)))) #620
  #vs Zariel
  mean(replicate(10000, sum(turno1_haste(great_axe(TRUE, TRUE), TRUE, TRUE, TRUE)))) #418
  #vs Zariel con arma luz luna
  mean(replicate(10000, sum(turno1_haste(great_axe(TRUE, TRUE), FALSE, TRUE, TRUE)))) #484

###stirring_dragon_great_axe
##SIN HASTE
  #Normal
  mean(replicate(10000, sum(turno1_no_haste(stirring_dragon_great_axe(TRUE, TRUE), FALSE, FALSE, FALSE)))) #573
  #vs Zariel
  mean(replicate(10000, sum(turno1_no_haste(stirring_dragon_great_axe(TRUE, TRUE), FALSE, TRUE, TRUE)))) #457

##CON HASTE
  #Normal
  mean(replicate(10000, sum(turno1_haste(stirring_dragon_great_axe(TRUE, TRUE), FALSE, FALSE, FALSE)))) #677
  #vs Zariel 
  mean(replicate(10000, sum(turno1_haste(stirring_dragon_great_axe(TRUE, TRUE), FALSE, TRUE, TRUE)))) #540

###wakened_dragon_great_axe
##SIN HASTE
  #Normal
  mean(replicate(10000, sum(turno1_no_haste(wakened_dragon_great_axe(TRUE, TRUE), FALSE, FALSE, FALSE)))) #619
  #vs Zariel
  mean(replicate(10000, sum(turno1_no_haste(wakened_dragon_great_axe(TRUE, TRUE), FALSE, TRUE, TRUE)))) #503
  
##CON HASTE
  #Normal
  mean(replicate(10000, sum(turno1_haste(wakened_dragon_great_axe(TRUE, TRUE), FALSE, FALSE, FALSE)))) #732
  #vs Zariel
  mean(replicate(10000, sum(turno1_haste(wakened_dragon_great_axe(TRUE, TRUE), FALSE, TRUE, TRUE)))) #595
  

###ascendant_dragon_great_axe
##SIN HASTE
  #Normal
  mean(replicate(10000, sum(turno1_no_haste(ascendant_dragon_great_axe(TRUE, TRUE), FALSE, FALSE, FALSE)))) #666
  #vs Zariel
  mean(replicate(10000, sum(turno1_no_haste(ascendant_dragon_great_axe(TRUE, TRUE), FALSE, TRUE, TRUE)))) #550
  
##CON HASTE
  #Normal
  mean(replicate(10000, sum(turno1_haste(ascendant_dragon_great_axe(TRUE, TRUE), FALSE, FALSE, FALSE)))) #788
  #vs Zariel
  mean(replicate(10000, sum(turno1_haste(ascendant_dragon_great_axe(TRUE, TRUE), FALSE, TRUE, TRUE)))) #651
  