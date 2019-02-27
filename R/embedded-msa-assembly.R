library(plyr)
library(reshape2)

NUM_POOL_A <- 38
NUM_POOL_B <- 30
NUM_MSA_PLATES <- 50

# Pool A:  Mfg plates selected from the 30 - 100 range
poolA <- expand.grid(PoolPlateID=1:NUM_POOL_A, PoolPlateStrip=2:5)

set.seed(107)
poolA_sample <- sample(seq_len(nrow(poolA)), 150)

# A couple of strips will be left over:
poolA_rem <- setdiff(seq_len(nrow(poolA)), poolA_sample)

mfg_sampling_A <- 
  cbind(MSAPlate=rep(seq_len(NUM_MSA_PLATES), each=3),
                 poolA[poolA_sample,])


# Create Pool B:
poolB <- expand.grid(PoolPlateID=(NUM_POOL_A+1):(NUM_POOL_A + NUM_POOL_B),
                     PoolPlateStrip=1:5)

set.seed(595)
poolB_sample_1 <- sample(seq_len(nrow(poolB)), 148)
# Add the left-over Pool A strips.  Gives 150 strips total.
poolB_2 <- rbind(poolA[poolA_rem,], poolB[poolB_sample_1,])

set.seed(219)
mfg_sampling_B <- 
  cbind(MSAPlate=rep(seq_len(NUM_MSA_PLATES), each=3),
        poolB_2[sample(nrow(poolB_2)),])


# Now merge the two:

msa_map_1 <- 
  rbind(cbind(mfg_sampling_A, Pool="A"),
        cbind(mfg_sampling_B, Pool="B"))
msa_map_2 <- msa_map_1[order(msa_map_1$MSAPlate),]


## Now check:

# All 50 MSA plates should be represented:
all(sort(unique(msa_map_2$MSAPlate))==1:50)
# PASS

# Each MSA plate should have 6 strips:
all(table(msa_map_2$MSAPlate)==6)
# PASS

# Each MSA plate should contain 3 strips from Pool A and 3 from Pool B:
table(msa_map_2[,c("MSAPlate", "Pool")])

# Check the sampling of the Mfg plates:
table(msa_map_2[,c("PoolPlateID", "Pool")])


## Everything checks out.  Now randomize the strips within the MSA plates:

sample(877)
msa_map_3 <- 
  ddply(msa_map_2, .(MSAPlate),
        function(df) {
          cbind(df, AssayStrip=sample(1:6))
        })

# No need to convert strips to columns quite yet...

# Now order for the randomization:
ord_col <- c("PoolPlateID", "Pool", "PoolPlateStrip", "MSAPlate", "AssayStrip")
ord_row <- do.call(order, as.list(msa_map_3)[ord_col])
msa_map_4 <- msa_map_3[ord_row, ord_col]


# !!! The plate map file has changed!  Don't regenerate!!!

if (!file.exists("../Plate maps/EmbeddedMSA/EmbeddedMSAAssemblyMap.csv")) {
  write.csv(msa_map_4, "../Plate maps/EmbeddedMSAAssemblyMap.csv", row.names=FALSE)
} else {
  warning("Plate map file already exists and may have changed.  Will not regenerate.")
}

