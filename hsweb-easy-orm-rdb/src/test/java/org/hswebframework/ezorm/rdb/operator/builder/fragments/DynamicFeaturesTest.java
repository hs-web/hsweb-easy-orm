package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import org.junit.Test;

import static org.junit.Assert.*;

public class DynamicFeaturesTest {


    @Test
    public void test() {
        long time = System.currentTimeMillis();
        for (int i = 0; i < 100_0000; i++) {
            RDBFeatureType.termsType.getFeatureId("eq");
        }
        System.out.println(System.currentTimeMillis() - time);


        time = System.currentTimeMillis();
        for (int i = 0; i < 100_0000; i++) {
            DynamicFeatures.lookup(RDBFeatureType.termsType, "eq");
        }
        System.out.println(System.currentTimeMillis() - time);


    }
}