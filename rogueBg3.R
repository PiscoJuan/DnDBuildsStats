#Critico: Covert Cowl, Shade Slayer, BloodTHirst, Undermountain King, Dead Shot, champion = 14:20
#Critico: Spell Sniper, Shade Slayer, BloodTHirst, Undermountain King, Dead Shot, champion = 14:20
#Critico: Covert Cowl,Spell Sniper, Shade Slayer, BloodTHirst, Undermountain King, Dead Shot, champion = 13:20

tirar_dado <- function(nside, ndice){
  sample(1:nside, ndice, replace = TRUE)
}

#Habilidades
sneak_attack <- function(roguelvl){
  tirar_dado(6, ceiling(roguelvl/2))
}
dread_ambusher <- tirar_dado(8, 1)
sharp_shooter <- 10

#Armas & Ataques
dead_shot <- function(prof, modifier, enemy_AC, critmin, autocrit){
  atk_roll <- tirar_dado(20, 1)
  damage <- 0
  if(autocrit == 1){
    atk_roll <- 20
  }
  if(atk_roll >= critmin){
    temp <- tirar_dado(8, 2)
    damage <- sum(temp) + 2 + modifier
  }else{
    #if NOT disadvantage
    atk_roll2 <- atk_roll+ 2*prof + modifier + 2
    if(atk_roll2 >= enemy_AC){
      temp <- tirar_dado(8, 1) 
      damage <- sum(temp) + 2 + modifier
    }
  }
  c(atk_roll, damage)
}
deadshot_adv <- function(prof, modifier, enemy_AC, critmin, autocrit){
  temp1 <- dead_shot(prof, modifier, enemy_AC, critmin, autocrit)
  temp2 <- dead_shot(prof, modifier, enemy_AC, critmin, autocrit)
  if(temp1[1] < temp2[1]){
    temp1 <- temp2
  }
  temp1
}
#Ropa
helldusk_gloves <- tirar_dado(6, 1)
flawed_helldusk_gloves <- tirar_dado(4, 1)

#Anillos
caustic_band <- 2
strange_conduit <- tirar_dado(4, 1) #SOLO SI SE CONCENTRA
shadow_cloaked <- tirar_dado(4, 1) #SOLO SI ENEMIGO EN OSCURIDAD



##############################################################################################
#Magical Thief Posting
#Build 1: Rogue 4 (Arcane Trickster), Ranger 5 (Gloom Stalker), Fighter 3 (Champion)
build1 <- function(prof, modifier, enemy_AC, critmin, numrondas){
  #Ronda 1
  dmg <- helldusk_gloves + shadow_cloaked + sharp_shooter
  dmg_crit <- helldusk_gloves + shadow_cloaked 
  sneak <- 0
  sneakCrit <- 0
  
  att1 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
  if(att1[2] != 0){
    att1[2] <- att1[2] + dmg
    if(att1[1] >= critmin){
      att1[2] <- att1[2] + dmg_crit 
      sneakCrit <- 1
    }
    sneak <- 1
  }
  att2 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
  if(att2[2] != 0){
    att2[2] <- att2[2] + dmg
    if(att2[1] >= critmin){
      att2[2] <- att2[2] + dread_ambusher + dmg_crit
      if(sneak != 1){
        sneakCrit <- 1
      }
    }
    sneak <- 1
  }
  att3 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
  if(att3[2] != 0){
    att3[2] <- att3[2] + dmg
    if(att3[1] >= critmin){
      att3[2] <- att3[2] + dmg_crit
      if(sneak != 1){
        sneakCrit <- 1
      }
    }
    sneak <- 1
  }
  if(sneakCrit == 1){
    ronda <- att1[2] + att2[2] + att3[2] + sum(sneak_attack(4)) + sum(sneak_attack(4))
  }else if(sneak == 1){
    ronda <- att1[2] + att2[2] + att3[2] + sum(sneak_attack(4))
  }else{
    ronda <- 0
  }
  total <- ronda
  
  #Ronda 2:numrondas
  for (var in 2:numrondas){
    sneak <- 0
    sneakCrit <- 0
    
    att1 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
    if(att1[2] != 0){
      att1[2] <- att1[2] + dmg
      if(att1[1] >= critmin){
        att1[2] <- att1[2] + dmg_crit 
        sneakCrit <- 1
      }
      sneak <- 1
    }
    att2 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
    if(att2[2] != 0){
      att2[2] <- att2[2] + dmg
      if(att2[1] >= critmin){
        att2[2] <- att2[2] + dmg_crit
        if(sneak != 1){
          sneakCrit <- 1
        }
      }
      sneak <- 1
    }
    if(sneakCrit == 1){
      ronda <- att1[2] + att2[2] + sum(sneak_attack(4)) + sum(sneak_attack(4))
    }else if(sneak == 1){
      ronda <- att1[2] + att2[2] + sum(sneak_attack(4))
    }else{
      ronda <- 0
    }
    total <- append(total, ronda)
  }
  total
}

#Etzio Auditorio
#Build 2: Rogue 3 (Assasin), Ranger 5 (Gloom Stalker), Fighter 4 (Champion)
build2 <- function(prof, modifier, enemy_AC, critmin, numrondas){
  #Ronda 1
  dmg <- helldusk_gloves + shadow_cloaked + sharp_shooter
  dmg_crit <- helldusk_gloves + shadow_cloaked 
  sneak <- 0
  sneakCrit <- 0
  
  att1 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 1)
  att1[2] <- att1[2] + dmg
  att1[2] <- att1[2] + dmg_crit 
  
  att2 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 1)
  att2[2] <- att2[2] + dmg
  att2[2] <- att2[2] + dread_ambusher + dmg_crit

  att3 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 1)
  att3[2] <- att3[2] + dmg
  att3[2] <- att3[2] + dmg_crit
  att4 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 1)
  att4[2] <- att4[2] + dmg
  att4[2] <- att4[2] + dmg_crit
  ronda <- att1[2] + att2[2] + att3[2] + att4[2] + sum(sneak_attack(4)) + sum(sneak_attack(4))
  total <- ronda
  
  #Ronda 2:numrondas
  for (var in 2:numrondas){
    sneak <- 0
    sneakCrit <- 0
    
    att1 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
    if(att1[2] != 0){
      att1[2] <- att1[2] + dmg
      if(att1[1] >= critmin){
        att1[2] <- att1[2] + dmg_crit 
        sneakCrit <- 1
      }
      sneak <- 1
    }
    att2 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
    if(att2[2] != 0){
      att2[2] <- att2[2] + dmg
      if(att2[1] >= critmin){
        att2[2] <- att2[2] + dmg_crit
        if(sneak != 1){
          sneakCrit <- 1
        }
      }
      sneak <- 1
    }
    if(sneakCrit == 1){
      ronda <- att1[2] + att2[2] + sum(sneak_attack(4)) + sum(sneak_attack(4))
    }else if(sneak == 1){
      ronda <- att1[2] + att2[2] + sum(sneak_attack(4))
    }else{
      ronda <- 0
    }
    total <- append(total, ronda)
  }
  total
}

#El rogue mas rogue
#Build 3: Ranger 5 (Gloom Stalker), Fighter 3 (Champion), Warlock 4 (Great Old One/Chain)
build3 <- function(prof, modifier, enemy_AC, critmin, numrondas){
  #Ronda 1
  dmg <- helldusk_gloves + shadow_cloaked + strange_conduit + sharp_shooter
  dmg_crit <- helldusk_gloves + shadow_cloaked + strange_conduit

  
  att1 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
  if(att1[2] != 0){
    att1[2] <- att1[2] + dmg
    if(att1[1] >= critmin){
      att1[2] <- att1[2] + dmg_crit
    }
  }
  att2 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
  if(att2[2] != 0){
    att2[2] <- att2[2] + dmg
    if(att2[1] >= critmin){
      att2[2] <- att2[2] + dread_ambusher + dmg_crit
    }
  }
  att3 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
  if(att3[2] != 0){
    att3[2] <- att3[2] + dmg
    if(att3[1] >= critmin){
      att3[2] <- att3[2] + dmg_crit
    }
  }

  ronda <- att1[2] + att2[2] + att3[2]
  total <- ronda
  
  #Ronda 2:numrondas
  for (var in 2:numrondas){
    sneak <- 0
    sneakCrit <- 0
    
    att1 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
    if(att1[2] != 0){
      att1[2] <- att1[2] + dmg
      if(att1[1] >= critmin){
        att1[2] <- att1[2] + dmg_crit
      }
    }
    att2 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
    if(att2[2] != 0){
      att2[2] <- att2[2] + dmg
      if(att2[1] >= critmin){
        att2[2] <- att2[2] + dmg_crit
      }
    }
    ronda <- att1[2] + att2[2]
    total <- append(total, ronda)
  }
  total
}

#El rogue mas rogue DE VERDAD
#Build 4: Rogue 3 (Assasin), Fighter 5 (Champion), Warlock 4 (Great Old One/Chain)
build4 <- function(prof, modifier, enemy_AC, critmin, numrondas){
  #Ronda 1
  dmg <- helldusk_gloves + shadow_cloaked + strange_conduit + sharp_shooter
  dmg_crit <- helldusk_gloves + shadow_cloaked + strange_conduit
  sneak <- 0
  sneakCrit <- 0
  
  att1 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 1)
  att1[2] <- att1[2] + dmg
  att1[2] <- att1[2] + dmg_crit 
  att3 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 1)
  att3[2] <- att3[2] + dmg
  att3[2] <- att3[2] + dmg_crit
  att4 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 1)
  att4[2] <- att4[2] + dmg
  att4[2] <- att4[2] + dmg_crit
  ronda <- att1[2] + att3[2] + att4[2] + sum(sneak_attack(4)) + sum(sneak_attack(4))
  total <- ronda
  
  #Ronda 2:numrondas
  for (var in 2:numrondas){
    sneak <- 0
    sneakCrit <- 0
    
    att1 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
    if(att1[2] != 0){
      att1[2] <- att1[2] + dmg
      if(att1[1] >= critmin){
        att1[2] <- att1[2] + dmg_crit 
        sneakCrit <- 1
      }
      sneak <- 1
    }
    att2 <- deadshot_adv(prof, modifier, enemy_AC, critmin, 0)
    if(att2[2] != 0){
      att2[2] <- att2[2] + dmg
      if(att2[1] >= critmin){
        att2[2] <- att2[2] + dmg_crit
        if(sneak != 1){
          sneakCrit <- 1
        }
      }
      sneak <- 1
    }
    if(sneakCrit == 1){
      ronda <- att1[2] + att2[2] + sum(sneak_attack(4)) + sum(sneak_attack(4))
    }else if(sneak == 1){
      ronda <- att1[2] + att2[2] + sum(sneak_attack(4))
    }else{
      ronda <- 0
    }
    total <- append(total, ronda)
  }
  total
}

n_rondas <- 4
crit <- 14
rowMeans(replicate(1000, build1(4,5,crit,40,n_rondas)))
mean(replicate(1000, sum(build1(4,5,crit,40,n_rondas))))
rowMeans(replicate(1000, build3(4,5,crit,40,n_rondas)))
mean(replicate(1000, sum(build3(4,5,crit,40,n_rondas))))
rowMeans(replicate(1000, build2(4,5,crit,40,n_rondas)))
mean(replicate(1000, sum(build2(4,5,crit,40,n_rondas))))
rowMeans(replicate(1000, build4(4,5,crit,40,n_rondas)))
mean(replicate(1000, sum(build4(4,5,crit,40,n_rondas))))


roll4stats<-function(){
  uno <- tirar_dado(6,1)
  dos <- tirar_dado(6,1)
  tres <- tirar_dado(6,1)
  cuatro <- tirar_dado(6,1)
  rolls <- c(uno, dos, tres, cuatro)
  res <- sum(rolls) - which.min(rolls)
  res
}
roll4stats()
median(replicate(10000, sum(roll4stats())))
