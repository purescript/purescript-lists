/* jshint node: true */
"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var run = require("gulp-run");
var rimraf = require("rimraf");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("make", function() {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscMake({ ffi: foreigns }));
});

gulp.task("docs", ["clean-docs"], function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.pscDocs({
      docgen: {
        "Data.List": "docs/Data.List.md",
        "Data.List.Lazy": "docs/Data.List.Lazy.md",
        "Data.List.Unsafe": "docs/Data.List.Unsafe.md",
        "Data.List.ZipList": "docs/Data.List.ZipList.md"
      }
    }));
});

gulp.task("dotpsci", function () {
  return gulp.src(sources)
    .pipe(plumber())
    .pipe(purescript.dotPsci());
});

gulp.task("test", ["make"], function() {
  return gulp.src(sources.concat(["test/**/*.purs"]))
    .pipe(plumber())
    .pipe(purescript.psc({ main: "Test.Main", ffi: foreigns }))
    .pipe(run("node"));
});

gulp.task("default", ["make", "docs", "dotpsci", "test"]);
