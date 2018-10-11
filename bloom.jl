
# Bloom filter
# N = number of items
# M = filter size in bits
# K = number of filters
# B = total filter size in bits

nocollision(M, N) = prod((M-N+1):M)
totalpaths(M, N) = M^N

goodprob1(M, N) = nocollision(M, N) / totalpaths(M, N)
collisionprob(M, N, K = 1) = (1 - goodprob1(M, N))^K

