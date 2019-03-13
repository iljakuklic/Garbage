
echo '# Each cell has only one number'
for rc in {1..4},{1..4}; do
  echo $rc:{1..4}
done

echo '# Each row has each number once'
for cn in {1..4}:{1..4}; do
  echo {1..4},$cn
done

echo '# Each column has each number once'
for r in {1..4}; do
  for n in {1..4}; do
    echo $r:{1..4}:$n
  done
done

echo '# Each block has each number once'
for n in {1..4}; do
  for blk in '1212' '1234' '3412' '3434'; do
    echo {'a,c','a,d','b,c','b,d'}:$n | tr abcd $blk
  done
done
