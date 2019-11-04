package org.hswebframework.ezorm.rdb.utils;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.meta.FeatureSupportedMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;

import java.util.function.Supplier;

public class ExceptionUtils {

    public static Throwable translation(FeatureSupportedMetadata metadata,Throwable e) {
        return metadata.findFeature(ExceptionTranslation.ID)
                .map(trans->trans.translate(e))
                .orElse(e);
    }

    @SneakyThrows
    public static <T> T translation(Supplier<T> supplier, FeatureSupportedMetadata metadata) {
       try{
           return supplier.get();
       }catch (Throwable r){
            throw translation(metadata,r);
       }
    }



}
