package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.core.FeatureType;
import org.hswebframework.ezorm.core.meta.Feature;

import java.util.*;
import java.util.stream.Collectors;

public class FeatureUtils {

    private static boolean r2dbcIsAlive;

    static {
        try {
            Class.forName("io.r2dbc.spi.Connection");
            r2dbcIsAlive = true;
        } catch (ClassNotFoundException e) {
            r2dbcIsAlive = false;
        }

    }

    public static boolean r2dbcIsAlive() {
        return r2dbcIsAlive;
    }


    public static String featureToString(List<Feature> features) {
        StringBuilder builder = new StringBuilder();

        Map<FeatureType, List<Feature>> featureMap = features
                .stream()
                .collect(Collectors.groupingBy(Feature::getType
                        , () -> new TreeMap<>(Comparator.comparing(FeatureType::getId))
                        , Collectors.toList()));

        for (Map.Entry<FeatureType, List<Feature>> entry : featureMap.entrySet()) {
            builder.append("--").append(entry.getKey().getId())
                    .append(" (").append(entry.getKey().getName())
                    .append(")")
                    .append("\n");
            for (Feature feature : entry.getValue()) {
                builder.append("-----|---- ")
                        .append(feature.getId())
                        .append(" (")
                        .append(feature.getName())
                        .append(")")
                        .append("\t").append(feature.getClass().getSimpleName())
                        .append("\n");
            }
        }

        return builder.toString();
    }

}
