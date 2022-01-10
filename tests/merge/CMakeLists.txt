
# Add Unit tests
ADD_GUILE_TEST(ConnectorMergeBasic     connector-merge-basic.scm)
ADD_GUILE_TEST(ConnectorMergeBasicTri  connector-merge-basic-tri.scm)
ADD_GUILE_TEST(ConnectorMergeCons      connector-merge-cons.scm)
ADD_GUILE_TEST(ConnectorMergeConInd    connector-merge-conind.scm)
ADD_GUILE_TEST(ConnectorMergeConExt    connector-merge-conext.scm)
ADD_GUILE_TEST(ConnectorMergeTri       connector-merge-tri.scm)
ADD_GUILE_TEST(ConnectorMergeTriCon    connector-merge-tricon.scm)
ADD_GUILE_TEST(ConnectorMergeTriConInd connector-merge-triconind.scm)

ADD_GUILE_TEST(ConnectorMergeConsDbl   connector-merge-cons-dbl.scm)
ADD_GUILE_TEST(ConnectorMergeTriConDbl connector-merge-tricon-dbl.scm)

ADD_GUILE_TEST(ClassMergeBasic         class-merge-basic.scm)
ADD_GUILE_TEST(ClassMergeCons          class-merge-cons.scm)
ADD_GUILE_TEST(ClassMergeCother        class-merge-cother.scm)

ADD_GUILE_TEST(ConnectorBalance        connector-balance.scm)
ADD_GUILE_TEST(ConnectorBalanceBoth    connector-balance-both.scm)
ADD_GUILE_TEST(BalanceBidonor          connector-balance-bidonor.scm)
ADD_GUILE_TEST(BalanceTridonor         connector-balance-tridonor.scm)

ADD_GUILE_TEST(ShapeMerge              shape-merge.scm)
