package org.hswebframework.ezorm.spring.mapping;

import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.mapping.EntityTableMetadataParser;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.spring.EntityTableMetadataResolver;
import org.hswebframework.ezorm.spring.executor.ReactiveTestApplication;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Service;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.Assert.*;


@SpringBootTest(classes = ReactiveTestApplication.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class EasyormRepositoryRegistrarTest {

    @Autowired
    DatabaseOperator operator;

    @Autowired
    private EntityTableMetadataResolver resolver;

    @Autowired
    private TransactionRepository repository;

    @Before
    public void before() {
        operator.sql()
                .sync()
                .execute(SqlRequests.of("drop table if exists test_entity"));

        operator.ddl()
                .createOrAlter(resolver.resolve(TestEntity.class))
                .commit()
                .reactive()
                .block();
    }


    @Test
    public void test() {
        repository.test()
                .as(StepVerifier::create)
                .expectNext(3)
                .verifyComplete();
    }


}