open OUnit2
       
let suite =
  "All tests" >:::
    [
      TestMorphismsAtoms.testMorphismSAtoms;
      TestMorphismsConjunctiveQueries.testMorphismSConjunctiveQuery;
      TestChase.testChase "testFiles/example3.txt";
      TestSigmaRedundancySuppression.testSigmaRedundancySuppression;
      TestEquiv.testEquiv;
   
      TestCase.testCase;                   
      TestCase.testCaseTDB;                   
      TestCase.testCaseBUD;
      TestCase.testCaseTDD;
       
      TestCase2.testCase;
      TestCase2.testCaseTDB;
      TestCase2.testCaseBUD;
      TestCase2.testCaseTDD;
      
      TestCase3.testCase;
      TestCase3.testCaseTDB;
      TestCase3.testCaseBUD;
      TestCase3.testCaseTDD;
       
      TestCase4.testCase;
      TestCase4.testCaseTDB;
      TestCase4.testCaseBUD;
      TestCase4.testCaseTDD; 
   ]

let _ =
  run_test_tt_main suite  
