package org.hswebframework.ezorm.rdb.utils;

import org.hswebframework.ezorm.core.meta.FeatureSupportedMetadata;
import org.hswebframework.ezorm.rdb.operator.ExceptionTranslation;
import org.junit.Assert;
import org.junit.Test;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import org.reactivestreams.Publisher;

import org.hswebframework.ezorm.core.meta.Feature;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

public class ExceptionUtilsBranchTest {

    @Test
    public void testTranslationWithFeatureAndMissingFeatureBranches() {
        FeatureSupportedMetadata metadata = new TestMetadata(new RuntimeException("translated"));
        RuntimeException source = new RuntimeException("source");
        Throwable translated = ExceptionUtils.translation(metadata, source);
        Assert.assertEquals("translated", translated.getMessage());

        FeatureSupportedMetadata empty = new TestMetadata(null);
        Assert.assertSame(source, ExceptionUtils.translation(empty, source));
    }

    @Test
    public void testTranslationSupplierAndPublisherBranches() {
        FeatureSupportedMetadata metadata = new TestMetadata(new IllegalStateException("boom"));
        try {
            ExceptionUtils.translation(() -> { throw new IllegalArgumentException("bad"); }, metadata);
            Assert.fail();
        } catch (Throwable err) {
            Assert.assertEquals("boom", err.getMessage());
        }

        Function<Publisher<String>, Publisher<String>> fn = ExceptionUtils.translation(metadata);
        Assert.assertEquals("boom", StepVerifierLike.blockError((Mono<String>) fn.apply(Mono.error(new IllegalArgumentException("bad")))).getMessage());
        Assert.assertEquals("boom", StepVerifierLike.blockError((Flux<String>) fn.apply(Flux.error(new IllegalArgumentException("bad")))).getMessage());
    }

    private static class TestMetadata implements FeatureSupportedMetadata {
        private final Map<String, Feature> features = new HashMap<>();
        private TestMetadata(Throwable translated) {
            if (translated != null) {
                features.put(ExceptionTranslation.ID.getId(), new ExceptionTranslation() {
                    @Override public Throwable translate(Throwable e) { return translated; }
                });
            }
        }
        @Override public Map<String, Feature> getFeatures() { return features; }
        @Override public void addFeature(Feature feature) { features.put(feature.getId(), feature); }
    }

    private static class StepVerifierLike {
        static Throwable blockError(Mono<?> mono) { try { mono.block(); throw new AssertionError(); } catch (Throwable e) { return e; } }
        static Throwable blockError(Flux<?> flux) { try { flux.blockLast(); throw new AssertionError(); } catch (Throwable e) { return e; } }
    }
}
